import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), proc, withCreateProcess, waitForProcess)

buildFrontend :: IO ()
buildFrontend = do
  putStrLn "Building frontend"
  let npmProc = (proc "npm" ["run", "build"]) { cwd = Just "frontend" }
  withCreateProcess npmProc $ \_ _ _ handle -> do
    status <- waitForProcess handle
    case status of
      ExitSuccess -> putStrLn "Frontend compiled"
      ExitFailure code -> putStrLn $ "Failed to build frontend (exit code: " ++ show code ++ ")"

preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook args flags = buildFrontend >>
  return emptyHookedBuildInfo

main :: IO ()
main = defaultMainWithHooks
  $ simpleUserHooks { preBuild = preBuildHook }
