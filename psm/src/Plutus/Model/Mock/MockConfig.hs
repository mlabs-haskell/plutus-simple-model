-- | Config for Mock ledger
module Plutus.Model.Mock.MockConfig (
  Stat (..),
  MockConfig (..),
  CheckLimits (..),
  defaultSlotConfig,
  defaultMockConfig,
  defaultAlonzo,
  defaultBabbageV1,
  defaultBabbageV2,
  defaultAlonzoParams,
  defaultBabbageParamsV1,
  defaultBabbageParamsV2,
  skipLimits,
  warnLimits,
  forceLimits,
  readMockConfig,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Simple.Ledger.TimeSlot (SlotConfig (..))
import Plutus.Model.Mock.ProtocolParameters
import Plutus.Model.Mock.Stat
import Prelude

-- | Config for the blockchain.
data MockConfig = MockConfig
  { mockConfigCheckLimits :: !CheckLimits
  -- ^ limits check mode
  , mockConfigLimitStats :: !Stat
  -- ^ TX execution resources limits
  , mockConfigProtocol :: !PParams
  -- ^ Protocol parameters
  , mockConfigNetworkId :: !Network
  -- ^ Network id (mainnet / testnet)
  , mockConfigSlotConfig :: !SlotConfig
  -- ^ Slot config
  }

data CheckLimits
  = -- | ignore TX-limits
    IgnoreLimits
  | -- | log TX to error log if it exceeds limits but accept TX
    WarnLimits
  | -- | reject TX if it exceeds the limits
    ErrorLimits
  deriving (Show)

-- | Default slot config
defaultSlotConfig :: SlotConfig
defaultSlotConfig =
  SlotConfig
    { scSlotLength = 1000 -- each slot lasts for 1 second
    , scSlotZeroTime = 0 -- starts at unix epoch start
    }

{- | Default Babbage era config. If we use this parameter
 then Alonzo era TXs will be used for testing
-}
defaultAlonzo :: MockConfig
defaultAlonzo = defaultMockConfig defaultAlonzoParams

{- | Default Babbage V1 era config. If we use this parameter
 then Babbage era TXs will be used for testing
-}
defaultBabbageV1 :: MockConfig
defaultBabbageV1 = defaultMockConfig defaultBabbageParamsV1

{- | Default Babbage V2 era config. If we use this parameter
 then Babbage era TXs will be used for testing
-}
defaultBabbageV2 :: MockConfig
defaultBabbageV2 = defaultMockConfig defaultBabbageParamsV2

-- | Default blockchain config.
defaultMockConfig :: PParams -> MockConfig
defaultMockConfig params =
  MockConfig
    { mockConfigLimitStats = mainnetTxLimits
    , mockConfigCheckLimits = ErrorLimits
    , mockConfigProtocol = params
    , mockConfigNetworkId = Mainnet
    , mockConfigSlotConfig = defaultSlotConfig
    }

-- | Do not check for limits
skipLimits :: MockConfig -> MockConfig
skipLimits cfg = cfg {mockConfigCheckLimits = IgnoreLimits}

-- | Warn on limits
warnLimits :: MockConfig -> MockConfig
warnLimits cfg = cfg {mockConfigCheckLimits = WarnLimits}

-- | Error on limits
forceLimits :: MockConfig -> MockConfig
forceLimits cfg = cfg {mockConfigCheckLimits = ErrorLimits}

{- | Read config for protocol parameters and form blockchain config.

 > readMockConfig protocolParametersFile
-}
readMockConfig :: FilePath -> IO MockConfig
readMockConfig paramsFile =
  defaultMockConfig {- TODO . setDefaultCostModel -} <$> readAlonzoParams paramsFile
