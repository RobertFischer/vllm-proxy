{-# OPTIONS_GHC -Wno-name-shadowing #-}

module This.Wreq
  ( get,
    getBS,
    getLBS,
    getT,
    getJSON,
    postJSON,
    postJSON_,
    mkAppOpts,
  )
where

import Data.Aeson as JSON
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq qualified as Wreq
import RIO.ByteString qualified as SBS
import RIO.ByteString.Lazy qualified as LBS
import Text.URI qualified as URI
import This
import This.Orphans ()

mkAppOpts :: (MonadIO m) => Natural -> m Wreq.Options
mkAppOpts timeoutMins = do
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
