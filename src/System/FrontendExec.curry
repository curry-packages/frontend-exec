------------------------------------------------------------------------------
-- | Author : Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Finn Teegen
--   Version: March 2021
--
-- This module contains operations to execute the front end of the
-- Curry system.
------------------------------------------------------------------------------

module System.FrontendExec
  (
  -- * Type of targets of the front end
    FrontendTarget(..)
  -- * Parameters of the front end and their manipulation
  , FrontendParams(..), defaultParams, rcParams
  , setQuiet, setExtended, setCpp, addDefinition, setDefinitions
  , setOverlapWarn, setFullPath, setHtmlDir, setOutDir, setLogfile
  , addTarget, setSpecials, setFrontendPath
  -- * Operations to call the front end
  , callFrontend, callFrontendWithParams, getFrontendCall
  ) where

import Curry.Compiler.Distribution 
                          ( curryCompiler, curryCompilerMajorVersion
                          , curryCompilerMinorVersion, installDir
                          )
import Data.Char          ( toUpper )
import Data.List          ( intercalate, nub )
import Data.PropertyFile  ( getPropertiesFromFile )
import System.FilePath    ( FilePath, (</>), takeDirectory, takeFileName )
import System.Process     ( system )
import System.CurryPath   ( curryrcFileName, currySubdir, getLoadPathForModule )

-------------------------------------------------------------------
-- calling the front end
-------------------------------------------------------------------

-- | Data type for representing the different target files that can be produced
--   by the front end of the Curry compiler.
data FrontendTarget =
    FCY   -- ^ FlatCurry file ending with `.fcy`
  | TFCY  -- ^ Typed FlatCurry file ending with `.tfcy`
  | FINT  -- ^ FlatCurry interface file ending with `.fint`
  | ACY   -- ^ AbstractCurry file ending with `.acy`
  | UACY  -- ^ Untyped (without type checking) AbstractCurry file ending
          --   with `.uacy`
  | HTML  -- ^ colored HTML representation of source program
          --   (written to `M_curry.html` for source module `M.curry`)
  | CY    -- ^ source representation employed by the frontend ending with `.cy`
  | TOKS  -- ^ token stream of source program ending with `.tokens`
  | TAFCY -- ^ Type Annotated FlatCurry file ending with `.tafcy`
  | AST   -- ^ abstract syntax tree ending with `.ast`
  | SAST  -- ^ shortened abstract syntax tree ending with `.sast`
  | COMMS -- ^ comments stream ending with `.cycom`
  deriving (Eq, Show)

-- | Abstract data type for representing parameters supported by the front end
--   of the Curry compiler.
--   The parameters are of the form
--   `FrontendParams Quiet Extended Cpp NoOverlapWarn FullPath HtmlDir LogFile Specials FrontendPath`
data FrontendParams = FrontendParams
  { quiet         :: Bool             -- ^ work silently
  , extended      :: Bool             -- ^ support extended Curry syntax
  , cpp           :: Bool             -- ^ enable conditional compiling
  , definitions   :: [(String, Int)]  -- ^ definitions for conditional compiling
  , overlapWarn   :: Bool             -- ^ warn for overlapping rules
  , fullPath      :: Maybe [String]   -- ^ the complete list of directory names
                                      --   for loading modules
  , htmldir       :: Maybe String     -- ^ output directory for HTML target
  , outdir        :: String           -- ^ output directory for Curry artifacts
  , logfile       :: Maybe String     -- ^ store all output (including errors)
                                      --   of the front end in file
  , targets       :: [FrontendTarget] -- ^ additional targets for the front end
  , specials      :: String           -- ^ additional special parameters
                                      --   (use with care!)
  , frontendPath  :: String           -- ^ the path to the frontend executable
  }

-- | The default parameters of the front end.
defaultParams :: FrontendParams
defaultParams =
  FrontendParams
    { quiet        = False
    , extended     = True
    , cpp          = False
    , definitions  = defaultDefs
    , overlapWarn  = True
    , fullPath     = Nothing
    , htmldir      = Nothing
    , outdir       = currySubdir
    , logfile      = Nothing
    , targets      = []
    , specials     = ""
    , frontendPath = installDir </> "bin" </> curryCompiler ++ "-frontend"
    }
 where
  defaultDefs = [("__" ++ map toUpper curryCompiler ++ "__",
                  curryCompilerMajorVersion * 100 + curryCompilerMinorVersion)]

-- | The default parameters of the front end as configured by the compiler
--   specific resource configuration file.
rcParams :: IO FrontendParams
rcParams = do
  rcfile <- curryrcFileName
  [mbExtended,mbOverlapWarn] <- getPropertiesFromFile rcfile
                                  ["curryextensions","warnoverlapping"]
  return $ setExtended    (mbExtended    /= Just "no")
         $ setOverlapWarn (mbOverlapWarn /= Just "no")
         $ defaultParams

-- | Set quiet mode of the front end.
setQuiet :: Bool -> FrontendParams -> FrontendParams
setQuiet s ps = ps { quiet = s }

-- | Set extended mode of the front end.
setExtended :: Bool -> FrontendParams -> FrontendParams
setExtended s ps = ps { extended = s }

-- | Set cpp mode of the front end.
setCpp :: Bool -> FrontendParams -> FrontendParams
setCpp s ps = ps { cpp = s }

-- | Add cpp definition of the front end.
addDefinition :: (String, Int) -> FrontendParams -> FrontendParams
addDefinition d ps = ps { definitions = definitions ps ++ [d] }

-- | Set cpp definitions of the front end.
setDefinitions :: [(String, Int)] -> FrontendParams -> FrontendParams
setDefinitions s ps = ps { definitions = s }

-- | Set overlap warn mode of the front end.
setOverlapWarn :: Bool -> FrontendParams -> FrontendParams
setOverlapWarn s ps = ps { overlapWarn = s }

-- | Set the full path of the front end.
--   If this parameter is set, the front end searches all modules
--   in this path (instead of using the default path).
setFullPath :: [String] -> FrontendParams -> FrontendParams
setFullPath s ps = ps { fullPath = Just s }

-- | Set the htmldir parameter of the front end.
--   Relevant for HTML generation.
setHtmlDir :: String -> FrontendParams -> FrontendParams
setHtmlDir s ps = ps { htmldir = Just s }

-- | Sets the output directory of frontend artifacts ('currySubdir' by default)
setOutDir :: String -> FrontendParams -> FrontendParams
setOutDir s ps = ps { outdir = s }

-- | Set the logfile parameter of the front end.
--   If this parameter is set, all messages produced by the front end
--   are stored in this file.
setLogfile :: String -> FrontendParams -> FrontendParams
setLogfile s ps = ps { logfile = Just s }

-- | Set additional specials parameters of the front end.
--   These parameters are specific for the current front end and
--   should be used with care, since their form might change in the future.
setSpecials :: String -> FrontendParams -> FrontendParams
setSpecials s ps = ps { specials = s }

-- | Add an additional front end target.
addTarget :: FrontendTarget -> FrontendParams -> FrontendParams
addTarget t ps = ps { targets = t : targets ps }

-- | Sets the path to the frontend executable.
setFrontendPath :: String -> FrontendParams -> FrontendParams
setFrontendPath s ps = ps { frontendPath = s }

-- | In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--   are up to date, one can call the front end of the Curry compiler
--   with this action.
--   If the front end returns with an error, an exception is raised.
callFrontend :: FrontendTarget -- ^ the kind of target file to be generated
             -> String         -- ^ the name of the main module to be compiled
             -> IO ()
callFrontend target p = do
  params <- rcParams
  callFrontendWithParams target params p

-- | In order to make sure that compiler generated files
--   (like .fcy, .fint, .acy) are up to date,
--   one can call the front end of the Curry compiler
--   with this action where various parameters can be set.
--   If the front end returns with an error, an exception is raised.
callFrontendWithParams ::
     FrontendTarget -- ^ the kind of target file to be generated
  -> FrontendParams -- ^ parameters for the front end
  -> String         -- ^ the name of the main module to be compiled
                    --   (possibly prefixed with a directory
                    --   where this module resides)
  -> IO ()
callFrontendWithParams target params modpath = do
  syscall <- getFrontendCall target params modpath
  let lf = maybe "" id (logfile params)
  retcode <- if null lf
               then system syscall
               else system (syscall ++ " > " ++ lf ++ " 2>&1")
  if retcode == 0
    then return ()
    else ioError (userError "Illegal source program")

-- | Returns the system command invoked to call the front end of the
--   Curry compiler where various parameters can be set.
getFrontendCall :: FrontendTarget -- ^ the kind of target file to be generated
                -> FrontendParams -- ^ parameters for the front end
                -> String         -- ^ the name of the main module possibly
                                  --   prefixed with a directory where
                                  --   the module resides
                -> IO String      -- ^ system command to invoke the front end
getFrontendCall target params modpath = do
  parsecurry <- callParseCurry
  let tgts = nub (target : targets params)
  return $ unwords $ [parsecurry] ++ map showFrontendTarget tgts ++
                     [showFrontendParams, cppParams, takeFileName modpath]
 where
   callParseCurry = do
     path <- maybe (getLoadPathForModule modpath)
                   (\p -> return (nub (takeDirectory modpath : p)))
                   (fullPath params)
     return $ quote (frontendPath params) ++
              concatMap ((" -i" ++) . quote) path

   quote s = '"' : s ++ "\""

   showFrontendTarget FCY   = "--flat"
   showFrontendTarget TFCY  = "--typed-flat"
   showFrontendTarget TAFCY = "--type-annotated-flat --flat" -- due to f.e.bug
   showFrontendTarget FINT  = "--flat"
   showFrontendTarget ACY   = "--acy"
   showFrontendTarget UACY  = "--uacy"
   showFrontendTarget HTML  = "--html"
   showFrontendTarget CY    = "--parse-only"
   showFrontendTarget TOKS  = "--tokens"
   showFrontendTarget AST   = "--ast"
   showFrontendTarget SAST  = "--short-ast"
   showFrontendTarget COMMS = "--comments"

   showFrontendParams = unwords
    [ "-o ", outdir params
    , if quiet       params then runQuiet     else ""
    , if extended    params then "--extended" else ""
    , if cpp         params then "--cpp"      else ""
    , if overlapWarn params then ""           else "--no-overlap-warn"
    , maybe "" ("--htmldir="++) (htmldir params)
    , specials params
    , if withNewtypeDesugar && target `elem` [FCY,TFCY,TAFCY,FINT]
        then "-Odesugar-newtypes" -- remove newtypes by front end
        else ""
    ]

   runQuiet = "--no-verb --no-warn --no-overlap-warn"

   cppParams = intercalate " " $ map showDefinition (definitions params)

   showDefinition (s, v) = "-D" ++ s ++ "=" ++ show v

   withNewtypeDesugar =
     curryCompiler == "pakcs" && curryCompilerMajorVersion <= 3 &&
     curryCompilerMinorVersion < 4

------------------------------------------------------------------------------
