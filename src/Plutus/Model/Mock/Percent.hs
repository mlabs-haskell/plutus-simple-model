module Plutus.Model.Mock.Percent(
  Percent(..),
  toPercent,
  PercentExecutionUnits(..),
  StatPercent(..),
) where

import Prelude

-- | Percent values from 0 to 100 %.
newtype Percent = Percent { getPercent :: Float }
  deriving (Show, Eq)

-- | Convert integer to percent based on maximum value (first argument)
toPercent :: Integer -> Integer -> Percent
toPercent maxLim n = Percent $ (fromInteger @Float $ 100 * n ) / fromInteger maxLim

data PercentExecutionUnits = PercentExecutionUnits
  { percentExecutionSteps  :: !Percent
  , percentExecutionMemory :: !Percent
  }
  deriving (Show, Eq)

-- | Stats measured in percents (0 to 100 %)
data StatPercent = StatPercent
  { statPercentSize           :: !Percent
  , statPercentExecutionUnits :: !PercentExecutionUnits
  }
  deriving (Show, Eq)
