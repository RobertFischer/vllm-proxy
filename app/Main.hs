module Main (main, ensureAbsolute) where

import Data.Text.Lazy.Builder qualified as TB
import Data.Yaml qualified as YAML
import Katip qualified as K
import Katip.Scribes.Handle qualified as K
import Main.Opts
import Main.Types
import Paths_vllm_proxy qualified
import RIO.Process
import RIO.Text qualified as T
import Run
import System.IO qualified as IO
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
  pc <- mkDefaultProcessContext
  wreqOpts <- Wreq.mkAppOpts optTimeoutMins
  atomicStdGen <- initStdGen >>= newAtomicGenM
  kVerbo <- logDetail optLogFormat optVerbosity
  scribe <-
    K.mkHandleScribeWithFormatter
      (logFormat optLogFormat)
      K.ColorIfTerminal
      stdout
      (K.permitItem $ logLevel optVerbosity)
      kVerbo
  logEnv <-
    K.registerScribe "stdout" scribe K.defaultScribeSettings
      =<< K.initLogEnv
        (K.Namespace ["vLLM-Proxy"])
        (K.Environment $ T.pack version)
  let app =
        App
          { appProcessContext = pc,
            appWreqOpts = wreqOpts,
            appGenM = atomicStdGen,
            appLogCtx = mempty,
            appLogNs = mempty,
            appLogEnv = logEnv
          }
   in do
        runRIO app $ run cmd

logLevel :: Verbosity -> K.Severity
logLevel = \case
  VerbosityLoudest -> K.DebugS
  VerbosityLouder -> K.DebugS
  VerbosityLoud -> K.DebugS
  VerbosityDebug -> K.DebugS
  VerbosityInfo -> K.InfoS
  VerbosityNotice -> K.NoticeS
  VerbosityWarn -> K.WarningS
  VerbosityError -> K.ErrorS

logDetail :: LogFormat -> Verbosity -> IO K.Verbosity
logDetail fmt verb =
  IO.hIsTerminalDevice stdout <&> \case
    True -> defVerb
    False ->
      case fmt of
        BracketLogFormat -> defVerb
        _ -> K.V3
  where
    defVerb =
      case verb of
        VerbosityLoudest -> K.V3
        VerbosityLouder -> K.V2
        VerbosityLoud -> K.V1
        _ -> K.V0

logFormat :: (K.LogItem a) => LogFormat -> K.ItemFormatter a
logFormat = \case
  BracketLogFormat -> bracketLogFormat
  YAMLLogFormat -> yamlLogFormat
  JSONLogFormat -> jsonLogFormat

ensureAbsolute :: URI -> IO ()
ensureAbsolute uri =
  unless
    (URI.isPathAbsolute uri)
    (fail $ "URI must be absolute but is: " <> URI.renderStr uri)

kItemVerbosity :: K.Item a -> K.Verbosity -> K.Verbosity
kItemVerbosity i defVerb =
  if K._itemSeverity i < K.ErrorS then defVerb else K.V3

jsonLogFormat :: (K.LogItem a) => K.ItemFormatter a
jsonLogFormat withColor rawVerb i =
  K.jsonFormat withColor (kItemVerbosity i rawVerb) i

bracketLogFormat :: (K.LogItem a) => K.ItemFormatter a
bracketLogFormat withColor rawVerb i =
  K.bracketFormat withColor (kItemVerbosity i rawVerb) i

yamlLogFormat :: (K.LogItem a) => K.ItemFormatter a
yamlLogFormat withColor rawVerb i =
  TB.fromText
    . K.colorBySeverity withColor (K._itemSeverity i)
    . decodeUtf8Lenient
    . YAML.encode
    $ K.itemJson (kItemVerbosity i rawVerb) i
