import Control.Monad (unless)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist)
import System.Process (CreateProcess(..), proc, withCreateProcess, waitForProcess)

npmProc :: [String] -> String -> String -> IO ()
npmProc args success error = do
  let npmProc = (proc "npm" args) { cwd = Just "frontend" }
  withCreateProcess npmProc $ \_ _ _ handle -> do
    status <- waitForProcess handle
    case status of
      ExitSuccess -> putStrLn success
      ExitFailure code -> putStrLn $ error ++ " (exit code: " ++ show code ++ ")"

npmInstall :: IO ()
npmInstall = do
  putStrLn "Installing frontend dependencies"
  npmProc ["install"] "Dependencies installed" "Failed to install dependencies"

buildFrontend :: IO ()
buildFrontend = do
  putStrLn "Building frontend"
  npmProc ["run", "build"] "Fronted compiled" "Failed to build frontend"

preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook args flags = do
  modulesDir <- doesDirectoryExist $ "frontend" </> "node_modules"
  unless modulesDir npmInstall
  buildFrontend
  return emptyHookedBuildInfo

main :: IO ()
main = defaultMainWithHooks
  $ simpleUserHooks { preBuild = preBuildHook }
