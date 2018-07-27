import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Process (callProcess)

buildFrontend :: IO ()
buildFrontend = do
  putStrLn "Building frontend"
  -- callProcess "npm" ["build"]

preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook args flags = do
  buildFrontend
  return emptyHookedBuildInfo

main :: IO ()
main = defaultMainWithHooks
  $ simpleUserHooks { preBuild = preBuildHook }
