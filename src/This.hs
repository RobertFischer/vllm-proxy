{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module This
  ( module RIO,
    module This.Types,
    module This,
    module Control.Monad.Extra,
    module This.Orphans,
    module Control.Monad,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Extra (ifM, whenJust)
import Data.ByteString.Char8 qualified as C8
import RIO
import RIO.ByteString qualified as SBS
import RIO.Partial qualified as Bad
import System.Random.Stateful
import System.Time.Extra qualified as Time
import This.Orphans ()
import This.Types

-- | Safe version of 'Bad.pred' for 'Enum'.
pred :: (Enum a, Bounded a, Eq a) => a -> Maybe a
pred a =
  if a == minBound then Nothing else Just (Bad.pred a)

-- | Like 'pred', but returns the argument if it is already the 'minBound'.
pred' :: (Enum a, Bounded a, Eq a) => a -> a
pred' a = fromMaybe a $ pred a

-- | A safe version of 'Bad.pred' like 'pred'', but exchange the 'Bounded' and 'Eq' constraints for a
-- 'MonadUnliftIO' context.
predM :: (Enum a, MonadUnliftIO m) => a -> m (Maybe a)
predM a =
  either (const Nothing) Just <$> tryAny (evaluate $ Bad.pred a)

-- | Like 'predM', but returns the argument if a previous value cannot be calculated.
predM' :: (Enum a, MonadUnliftIO m) => a -> m a
predM' a = fromMaybe a <$> predM a

-- | Safe version of 'Bad.succ' for 'Enum'.
succ :: (Enum a, Bounded a, Eq a) => a -> Maybe a
succ a =
  if a == maxBound then Nothing else Just (Bad.succ a)

-- | Like 'succ', but returns the argument if it is already the 'minBound'.
succ' :: (Enum a, Bounded a, Eq a) => a -> a
succ' a = fromMaybe a $ succ a

-- | A safe version of 'Bad.succ' like 'succ'', but exchange the 'Bounded' and
-- 'Eq' constraints for a 'MonadUnliftIO' context.
succM :: (Enum a, MonadUnliftIO m) => a -> m (Maybe a)
succM a =
  either (const Nothing) Just <$> tryAny (evaluate $ Bad.succ a)

-- | Like 'succM', but returns the argument if a previous value cannot be
-- calculated.
succM' :: (Enum a, MonadUnliftIO m) => a -> m a
succM' a = fromMaybe a <$> succM a

-- | Prints a strict bytestring to standard out with an appended '\n' (UTF-8).
putStrLn :: (MonadIO m) => SBS.ByteString -> m ()
putStrLn = liftIO . C8.putStrLn

-- | Sleeps for the given number of seconds. The argument does not have to be a
-- whole number.
sleep :: (MonadIO m) => Time.Seconds -> m ()
sleep = liftIO . Time.sleep

-- | Generates a random natural number with the given upper bound.
randNat :: Word64 -> RApp Natural
randNat upbnd = do
  appGen <- appGenM <$> ask
  fromIntegral <$> uniformWord64R upbnd appGen

-- | Generates a random Word32 number with the given upper bound.
randWord32 :: Word32 -> RApp Word32
randWord32 upbnd = do
  appGen <- appGenM <$> ask
  uniformWord32R upbnd appGen

-- | Like 'putStrLn', but for strings.
printStrLn :: (MonadIO m) => String -> m ()
printStrLn = putStrLn . C8.pack

-- | Provides the arithmetic mean
mean :: (Foldable f, Real n) => f n -> Rational
mean ns = toRational (sum ns) / toRational (length ns)

-- | Repeats a given list a certain number of times.
cycleN :: Natural -> [a] -> [a]
cycleN 0 _ = []
cycleN n lst = lst <> cycleN (n - 1) lst -- Is it better to flip this?
