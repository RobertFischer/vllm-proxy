module Main.Opts
  ( module Options.Applicative.Simple,
    InitM,
    mkGlobalOptsParser,
    readmeCmdOpts,
    eitherReaderT,
    uriReadM,
  )
where

import Main.Types
import Options.Applicative
import Options.Applicative.Simple
import RIO.Process
import RIO.Text qualified as T
import System.IO qualified as IO
import Text.URI qualified as URI
import This

-- | Given an env var name and a default value, returns either
-- the environment variable name or the default value.
envOrDefault :: Text -> Text -> InitM Text
envOrDefault envVarName envDefVal = fromMaybe envDefVal <$> lookupEnvFromContext envVarName

-- | Provides the count of the number of times the given flag is called.
flagCnt :: Mod FlagFields () -> Parser Int
flagCnt mods = length <$> many (flag' () mods)

-- | Creates a verbosity option, which consists of both "-v" and "-q" and the calculation of their difference.
-- The default value can be set by the "VLLM_PXY_VERBOSITY" environment variable: options are "loud", "debug",
-- "info", "warn", or "error".  The "-v" and "-q" flags will calculate from there.
mkVerbosityOpt :: InitM (Parser Verbosity)
mkVerbosityOpt = do
  defVal <- calculateDefault . T.uncons <$> envOrDefault "VLLM_PXY_VERBOSITY" "Info"
  return $ vqParser defVal
  where
    defDefVal = VerbosityInfo -- Default default value
    vParser =
      flagCnt
        $ short 'v'
        <> long "verbose"
        <> help "Increase verbosity; may be specified multiple times; opposite of -q"
    qParser =
      flagCnt
        $ short 'q'
        <> long "quiet"
        <> help "Decrease verbosity; may be specified multiple times; opposite of -v"
    vqParser defVal = verbosityCalc defVal <$> liftA2 (,) vParser qParser
    verbosityCalc defVal = \case
      (0, 0) -> defVal
      (vs, qs) -> verbosityCalcLoop defVal $ qs - vs
    verbosityCalcLoop curVal c =
      if
        | c < 0 -> verbosityCalcLoop (pred' curVal) (c + 1)
        | c > 0 -> verbosityCalcLoop (succ' curVal) (c - 1)
        | otherwise -> curVal
    calculateDefault = \case
      Nothing -> defDefVal
      Just (c, _) -> case c of
        -- "1"
        '1' -> VerbosityLoudest
        -- "Loud"
        'L' -> VerbosityLoudest
        'l' -> VerbosityLoudest
        -- "Verbose"
        'V' -> VerbosityLoud
        'v' -> VerbosityLoud
        -- "Debug"
        'D' -> VerbosityDebug
        'd' -> VerbosityDebug
        -- "Info"
        'I' -> VerbosityInfo
        'i' -> VerbosityInfo
        -- "Warn"
        'W' -> VerbosityWarn
        'w' -> VerbosityWarn
        -- "Error"
        'E' -> VerbosityError
        'e' -> VerbosityError
        -- "Quiet"
        'Q' -> VerbosityError
        'q' -> VerbosityError
        -- "0"
        '0' -> VerbosityError
        -- Whatever...
        _ -> defDefVal

eitherReaderT :: (Text -> Either String a) -> ReadM a
eitherReaderT f = eitherReader (f . T.pack)

uriReadM :: ReadM URI
uriReadM = eitherReaderT URI.mkURI

-- | Makes a 'Parser' for 'GlobalOpts'
mkGlobalOptsParser :: InitM (Parser GlobalOpts)
mkGlobalOptsParser = do
  verbosityParser <- mkVerbosityOpt
  logFormatParser <- mkLogFormatParser
  return
    $ GlobalOpts
    <$> verbosityParser
    <*> timeoutMinsParser
    <*> logFormatParser

mkLogFormatParser :: InitM (Parser LogFormat)
mkLogFormatParser = do
  defaultDefaultFormat <-
    liftIO (IO.hIsTerminalDevice stdout) <&> \case
      True -> BracketLogFormat
      False -> JSONLogFormat
  defaultFormat <- calculateDefault defaultDefaultFormat . T.uncons <$> envOrDefault "VLLM_PXY_LOGFORMAT" ""
  return
    $ option
      logFormatReadM
      ( short 'L'
          <> long "log-format"
          <> metavar "SUFFIX"
          <> help "The file suffix for the logging format to use: 'json', 'yaml', or 'txt'"
          <> value defaultFormat
          <> showDefaultWith
            ( \case
                YAMLLogFormat -> "yaml"
                JSONLogFormat -> "json"
                BracketLogFormat -> "txt"
            )
      )
  where
    charToMaybeFmt = \case
      'y' -> Just YAMLLogFormat
      'Y' -> Just YAMLLogFormat
      'j' -> Just JSONLogFormat
      'J' -> Just JSONLogFormat
      't' -> Just BracketLogFormat
      'T' -> Just BracketLogFormat
      _ -> Nothing
    calculateDefault defFmt = fromMaybe defFmt . (>>= \(c, _) -> charToMaybeFmt c)
    logFormatReadM = maybeReader $ \case
      [] -> Nothing
      c : _ -> charToMaybeFmt c

timeoutMinsParser :: Parser Natural
timeoutMinsParser =
  option
    auto
    ( short 'T'
        <> long "http-timeout"
        <> help "HTTP request timeout in minutes"
        <> metavar "MINS"
        <> value 5
        <> showDefault
    )

readmeCmdOpts :: Parser ReadmeCmd
readmeCmdOpts = ReadmeCmd' <$> readmeDisplayOpt
  where
    readmeDisplayOpt = pure ReadmeDisplayPlain
