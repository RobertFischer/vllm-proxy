module Main (main, ensureAbsolute) where

import Katip qualified as K
import Main.Opts
import Main.Types
import Paths_vllm_proxy qualified
import RIO.Process
import RIO.Text qualified as T
import Run
import System.Random.Stateful
import Text.URI qualified as URI
import This
import This.Wreq qualified as Wreq

main :: IO ()
main = do
  let version = $(simpleVersion Paths_vllm_proxy.version)
  globalOpts <- runSimpleApp mkGlobalOptsParser
  (GlobalOpts {..}, cmd) <-
    simpleOptions
      version
      "vLLM Proxy"
      "A proxy for vLLM providing load balancing and other features.\n\nRun the 'help' command for more.\n"
      globalOpts
      $ do
        addCommand
          "help"
          "Print out the README"
          ReadmeCmd
          readmeCmdOpts
  lo <-
    logOptionsHandle stderr (optVerbosity == VerbosityLoud)
      <&> setLogMinLevel (logMinLevel optVerbosity)
  withLogFunc lo $ \lf -> do
    pc <- mkDefaultProcessContext
    wreqOpts <- Wreq.mkAppOpts optTimeoutMins
    atomicStdGen <- initStdGen >>= newAtomicGenM
    logEnv <-
      K.initLogEnv
        (K.Namespace ["vLLM-Proxy"])
        (K.Environment $ T.pack version)
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appWreqOpts = wreqOpts,
              appGenM = atomicStdGen,
              appLogCtx = mempty,
              appLogNs = mempty,
              appLogEnv = logEnv
            }
     in do
          runRIO app $ run cmd

ensureAbsolute :: URI -> IO ()
ensureAbsolute uri =
  unless
    (URI.isPathAbsolute uri)
    (fail $ "URI must be absolute but is: " <> URI.renderStr uri)

logMinLevel :: Verbosity -> LogLevel
logMinLevel = \case
  VerbosityLoud -> LevelDebug
  VerbosityDebug -> LevelDebug
  VerbosityInfo -> LevelInfo
  VerbosityWarn -> LevelWarn
  VerbosityError -> LevelError
