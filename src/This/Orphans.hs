{-# OPTIONS_GHC -Wno-orphans #-}

module This.Orphans () where

import Data.Prefix.Units
import GHC.Base (failIO)
import RIO
import RIO.Text qualified as T
import RIO.Time (NominalDiffTime)

-- It's important to mark these as INCOHERENT so that they are dropped if an
-- official implementation is available.
--
instance {-# INCOHERENT #-} RationalConvertible NominalDiffTime where
  convFromRational = fromRational

instance {-# INCOHERENT #-} Display Natural where
  textDisplay = T.pack . show

instance {-# INCOHERENT #-} RationalConvertible Natural where
  convFromRational = fromInteger . convFromRational

instance {-# INCOHERENT #-} (Show s) => Display s where
  textDisplay = T.pack . show

instance {-# INCOHERENT #-} MonadThrow (Either String) where
  throwM = Left . show

instance {-# INCOHERENT #-} MonadFail (Either String) where
  fail = Left

instance {-# INCOHERENT #-} (Monad m, MonadIO m) => MonadFail m where
  fail = liftIO . failIO
