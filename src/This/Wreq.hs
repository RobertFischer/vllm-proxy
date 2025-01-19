{-# OPTIONS_GHC -Wno-name-shadowing #-}

module This.Wreq
  ( get,
    getBS,
    getLBS,
    getT,
    getJSON,
    postJSON,
    postJSON_,
    vllmPath,
    countTokens,
    getCompletion,
    getCompletion',
    mkPiensoOpts,
    CompletionPrompt (..),
  )
where

import Data.Aeson as JSON
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq qualified as Wreq
import RIO.ByteString qualified as SBS
import RIO.ByteString.Lazy qualified as LBS
import RIO.Map qualified as M
import Text.URI qualified as URI
import Text.URI.QQ (uri)
import This
import This.Orphans ()

mkPiensoOpts :: (MonadIO m) => Natural -> m Wreq.Options
mkPiensoOpts timeoutMins = do
  httpMgr <-
    liftIO
      $ HTTP.newManager
        ( tlsManagerSettings
            { managerResponseTimeout = responseTimeoutMicro timeoutMicro
            }
        )
  return (Wreq.defaults & Wreq.manager .~ Right httpMgr)
  where
    timeoutMicro =
      fromIntegral timeoutMins
        * 60 -- from Seconds to Minutes
        * 1000 -- from Millis to Seconds
        * 1000 -- from Micros to Millis

-- | Gets the response.  Throws an error if the status code is not in the 2xx range.
get :: URI -> RApp (Wreq.Response LBS.ByteString)
get uri = do
  pOpts <- appWreqOpts <$> ask
  liftIO . Wreq.getWith pOpts $ URI.renderStr uri

-- | Returns the lazy ByteString of the request body.
getLBS :: URI -> RApp LBS.ByteString
getLBS = fmap (^. Wreq.responseBody) . get

-- | Returns the strict ByteString of the request body.
getBS :: URI -> RApp SBS.ByteString
getBS = fmap LBS.toStrict . getLBS

-- | Returns the Text of the request body. Assumes the content is UTF-8.
getT :: URI -> RApp Text
getT = fmap decodeUtf8Lenient . getBS

-- | Returns a value converted from JSON from the request body.
getJSON :: (FromJSON resp) => URI -> RApp resp
getJSON uri = do
  resp <- getLBS uri
  case JSON.eitherDecode resp of
    Left msg -> fail $ "Could not convert JSON from GET " <> URI.renderStr uri <> ": " <> msg
    Right val -> return val

-- | Does a POST with the given JSON content, returning the JSON response.
postJSON :: (ToJSON req, FromJSON resp) => URI -> req -> RApp resp
postJSON uri req = do
  pOpts <- appWreqOpts <$> ask
  httpResp <- liftIO $ Wreq.postWith pOpts uriStr (JSON.toJSON req)
  case httpResp ^? Wreq.responseBody of
    Nothing -> fail $ "No response body from call to POST " <> uriStr
    Just bs -> case JSON.eitherDecode bs of
      Left msg -> fail $ "Could not convert JSON from POST " <> uriStr <> ": " <> msg
      Right val -> return val
  where
    uriStr = URI.renderStr uri

-- | Does a POST with the given JSON content, ignoring the response beyond checking for 2xx.
postJSON_ :: (ToJSON req) => URI -> req -> RApp ()
postJSON_ uri req = do
  pOpts <- appWreqOpts <$> ask
  void
    . liftIO
    $ Wreq.postWith pOpts uriStr (JSON.toJSON req)
  where
    uriStr = URI.renderStr uri

-- | Constructs a URI that is the base URI appended with the given path.
vllmPath :: Text -> RApp URI
vllmPath path = URI.mkURI path >>= vllmPath'

vllmPath' :: URI -> RApp URI
vllmPath' pathAsUri = do
  vllmUri <- appBaseUri <$> ask
  case pathAsUri `URI.relativeTo` vllmUri of
    Nothing -> fail $ "Failed to resolve " <> URI.renderStr pathAsUri <> " relative to " <> URI.renderStr vllmUri
    Just v -> return v

countTokens :: Text -> RApp Natural
countTokens prompt = do
  modelName <- appModel <$> ask
  endpointUri <- vllmPath' [uri|/tokenize|]
  TokenCountResp result <- postJSON endpointUri $ TokenCountReq modelName prompt
  return result

newtype TokenCountResp = TokenCountResp Natural deriving (Eq, Ord, Show)

instance FromJSON TokenCountResp where
  parseJSON = withObject "TokenCountResp" $ \v ->
    TokenCountResp <$> v .: "count"

data TokenCountReq = TokenCountReq
  { tcreqModel :: Text,
    tcreqPrompt :: Text
  }
  deriving (Eq, Show)

instance ToJSON TokenCountReq where
  toJSON TokenCountReq {..} =
    toJSON
      $ M.singleton ("model" :: Text) tcreqModel
      <> M.singleton "prompt" tcreqPrompt

getCompletion :: Text -> Maybe Natural -> RApp Text
getCompletion prompt maxTokens =
  getCompletion'
    $ CompletionPrompt
      { promptMaxTokens = maxTokens,
        promptPrompt = prompt,
        promptTemperature = Just pct0,
        promptTopP = Just pct100,
        promptMinP = Just pct0,
        promptTopK = Nothing
      }

data CompletionPrompt = CompletionPrompt
  { promptMaxTokens :: Maybe Natural,
    promptPrompt :: Text,
    promptTemperature :: Maybe Percent,
    promptTopP :: Maybe Percent,
    promptMinP :: Maybe Percent,
    promptTopK :: Maybe Natural
  }
  deriving (Generic)

instance ToJSON CompletionPrompt

getCompletion' :: CompletionPrompt -> RApp Text
getCompletion' CompletionPrompt {..} = do
  modelName <- appModel <$> ask
  endpointUri <- vllmPath' [uri|/v1/completions|]
  let completionReq =
        CompletionReq
          { creqModel = modelName,
            creqPrompt = promptPrompt,
            creqMaxTokens = promptMaxTokens,
            creqTemperature = promptTemperature,
            creqMinP = promptMinP,
            creqTopP = promptTopP,
            creqTopK = promptTopK
          }
  logDebug . display $ completionReq
  CompletionResp result <- postJSON endpointUri completionReq
  logDebug $ display result
  return result

data CompletionReq = CompletionReq
  { creqModel :: Text,
    creqPrompt :: Text,
    creqMaxTokens :: Maybe Natural,
    creqTemperature :: Maybe Percent,
    creqTopP :: Maybe Percent,
    creqTopK :: Maybe Natural,
    creqMinP :: Maybe Percent
  }
  deriving (Show)

instance ToJSON CompletionReq where
  toJSON CompletionReq {..} =
    toJSON
      $ M.fromList @Text @JSON.Value
        [ ("model", toJSON creqModel),
          ("prompt", toJSON creqPrompt)
        ]
      <> maxTokens
      <> topK
      <> minP
      <> topP
      <> temperature
    where
      mayMap :: (ToJSON a) => Text -> Maybe a -> M.Map Text JSON.Value
      mayMap key = maybe M.empty (M.singleton key . toJSON)
      maxTokens = mayMap "max_tokens" creqMaxTokens
      topK = mayMap "top_k" creqTopK
      minP = mayMap "min_p" (realToFrac @_ @Float <$> creqMinP)
      topP = mayMap "top_p" (realToFrac @_ @Float <$> creqTopP)
      temperature = mayMap "temperature" (realToFrac @_ @Float <$> creqTemperature)

newtype CompletionResp = CompletionResp Text
  deriving (Show)

instance FromJSON CompletionResp where
  parseJSON = withObject "CompletionResp" $ \v -> do
    choices <- v .: "choices"
    items <- parseJSONList choices
    case items of
      [] -> fail $ "Empty 'choices' found: " <> show v
      item : _ -> CompletionResp <$> (item .: "text")
