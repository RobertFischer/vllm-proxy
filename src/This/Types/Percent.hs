module This.Types.Percent
  ( Percent,
    percent,
    truncateToPercent,
    roundToPercent,
    percentOf,
    pct0,
    pct1,
    pct50,
    pct100,
  )
where

import Data.Aeson qualified as JSON
import RIO
import RIO.Char qualified as C
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as RP
import Text.Read

-- | A type-safe representation of a whole percentage value. See 'percent',
-- 'truncateToPercent', and 'roundToPercent' for constructors. Note that
-- the 'fronInteger' implementation treats the integer at its word,
-- so 2 is 200%.
--
-- If you need fractional percentage values, just use 'Rational' for your
-- calculations and then either 'truncateToPercent' or 'roundToPercent'
newtype Percent
  = -- | Store just the numerator.
    Percent Integer
  deriving stock (Eq, Ord, Data)
  deriving (Enum, NFData, Hashable) via Integer

-- | Provides the rational value that is the percent of the second argument.
percentOf :: (Real n) => Percent -> n -> Rational
percentOf (Percent numer) n = toRational n * toRational numer / 100

-- | Given a value (eg: 23), provide the 'Percent' value represented
-- that value divided by 100 (eg: 23/100, aka: 23%).
percent :: (Integral i) => i -> Percent
percent = Percent . toInteger

-- | Gives the truncaqted (via 'floor') approximation of the argument as a percentage.
truncateToPercent :: (RealFrac f) => f -> Percent
truncateToPercent f = Percent . floor $ f * 100

-- | Gives the roudned approximation of the argument as a percentage.
roundToPercent :: (RealFrac f) => f -> Percent
roundToPercent f = Percent . round $ f * 100

instance Num Percent where
  (+) (Percent left) (Percent right) = Percent (left + right)
  (*) left right = roundToPercent $ toRational left * toRational right
  abs (Percent pct) = Percent (abs pct)
  signum (Percent pct) = Percent (signum pct)
  negate (Percent pct) = Percent (negate pct)
  fromInteger i = Percent (i * 100)

instance Real Percent where
  toRational (Percent pct) = toRational pct / 100

instance Show Percent where
  show (Percent pct) = show pct <> "%"

instance Read Percent where
  readPrec = RP.lift $ do
    ReadP.skipSpaces
    sign <- ReadP.option "" $ ReadP.string "-"
    ReadP.skipSpaces
    intStart <- ReadP.munch1 isNonzeroDigit
    intEnd <- ReadP.munch C.isDigit
    ReadP.skipSpaces
    _ <- ReadP.string "%"
    ReadP.skipSpaces
    let numStr = sign <> intStart <> intEnd
    case readMaybe @Integer numStr of
      Nothing -> fail $ "Could not parse integer part of percent: " <> numStr
      Just val -> return $ Percent val
    where
      isNonzeroDigit c = C.isDigit c && c /= '0'

instance JSON.ToJSON Percent where
  toJSON = JSON.toJSON . realToFrac @_ @Double

-- | Constant representing 0%
pct0 :: Percent
pct0 = percent 0

-- | Constant representing 1%
pct1 :: Percent
pct1 = percent 1

-- | Constant representing 50%
pct50 :: Percent
pct50 = percent 50

-- | Constant representing 100%
pct100 :: Percent
pct100 = percent 100
