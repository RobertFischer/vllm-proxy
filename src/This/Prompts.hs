module This.Prompts
  ( makePrompt,
  )
where

import Data.Bits (shiftR)
import RIO.File (writeBinaryFileDurableAtomic)
import RIO.FilePath ((</>))
import RIO.List qualified as L
import RIO.Text qualified as T
import Text.URI.QQ (uri)
import This
import This.Wreq qualified as Wreq
import UnliftIO.Directory

promptCacheDir :: RApp FilePath
promptCacheDir = do
  cacheDirPath <- getXdgDirectory XdgCache ("vllm" </> "prompts")
  createDirectoryIfMissing True cacheDirPath
  return cacheDirPath

promptCacheFile :: FilePath -> RApp FilePath
promptCacheFile fileName = promptCacheDir <&> (</> fileName)

-- | Provides some words from Shakespeare.
shakespeareWords :: RApp [Text]
shakespeareWords = do
  cacheFilePath <- promptCacheFile "shakespeare.txt"
  logDebug $ "Reading from cache file: " <> display cacheFilePath
  contents <-
    readFromFile cacheFilePath
      `catchAny` ( \ex -> do
                     logDebug $ display ex
                     logInfo "Fetching Shakespeare from web to populate cache"
                     fetchFromWeb cacheFilePath
                 )
  let allWords = filter (not . T.null) $ T.words contents
  dropCount <- randWord32 (max 1 . fromIntegral $ length allWords - 1)
  let result = drop (fromIntegral dropCount) allWords
  logDebug $ "Returning prompt source with " <> display (length result) <> " words"
  return result
  where
    readFromFile = readFileUtf8
    fetchFromWeb cacheFilePath = do
      contents <- Wreq.getT [uri|https://www.gutenberg.org/cache/epub/100/pg100.txt|]
      writeCacheFile cacheFilePath contents `catchAny` logCacheFailure cacheFilePath
      return contents
    writeCacheFile cacheFilePath contents = do
      writeBinaryFileDurableAtomic cacheFilePath (encodeUtf8 contents)
      logDebug $ "Populated cache file at: " <> display cacheFilePath
    logCacheFailure cacheFilePath ex =
      logWarn $ "Could not cache Shakespeare to " <> display cacheFilePath <> ": " <> display ex

-- | Constructs a prompt with the given number of tokens (or perhaps up to 2 fewer
-- tokens than requested). If the argument is 8 or smaller, the returned prompt may
-- nonetheless be up to 8 tokens in length.
makePrompt :: Natural -> RApp Text
makePrompt tokenCount =
  if tokenCount < minTokenCount
    then do
      logWarn
        $ "Bumping up prompt token count from "
        <> display tokenCount
        <> " to "
        <> display minTokenCount
      makePrompt minTokenCount
    else do
      logDebug "Generating a prompt"
      result <- doMakePrompt
      logDebug "Finished generating a prompt"
      return result
  where
    minTokenCount = 8
    initPrompt = "Continue this:\n"
    doMakePrompt = mkTokens initPrompt []
    mkTokens (current :: Text) (moreWords :: [Text]) =
      if L.null moreWords
        then shakespeareWords >>= mkTokens current
        else
          Wreq.countTokens current >>= \curCount ->
            if
              | tokenCount < curCount -> do
                  logWarn "Too many tokens generated for a prompt; restarting."
                  doMakePrompt
              | tokenCount <= curCount + 2 -> return current
              | otherwise ->
                  -- 'Words' (characters delimited by whitespace) might be multiple
                  -- tokens, so we creep up on the proper number of tokens here.
                  let wordsToTake = max 1 . fromIntegral $ shiftR (tokenCount - curCount) 3
                   in let (nextWords, restWords) = L.splitAt wordsToTake moreWords
                       in mkTokens (T.unwords (current : nextWords)) restWords
