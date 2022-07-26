module Plutus.Model.Mock.MockConfig(
  Stat(..),
  MockConfig(..),
  CheckLimits(..),
  defaultSlotConfig,
  defaultMockConfig,
  defaultAlonzo,
  defaultBabbage,
  defaultAlonzoParams,
  defaultBabbageParams,
  skipLimits,
  warnLimits,
  forceLimits,
  readMockConfig,
) where

import Prelude
import Cardano.Ledger.BaseTypes
import Plutus.Model.Fork.Ledger.TimeSlot (SlotConfig (..))
import Plutus.Model.Mock.ProtocolParameters
import Plutus.Model.Mock.Stat

-- | Config for the blockchain.
data MockConfig = MockConfig
  { mockConfigCheckLimits  :: !CheckLimits       -- ^ limits check mode
  , mockConfigLimitStats   :: !Stat              -- ^ TX execution resources limits
  , mockConfigProtocol     :: !PParams           -- ^ Protocol parameters
  , mockConfigNetworkId    :: !Network           -- ^ Network id (mainnet / testnet)
  , mockConfigSlotConfig   :: !SlotConfig        -- ^ Slot config
  }

data CheckLimits
  = IgnoreLimits   -- ^ ignore TX-limits
  | WarnLimits     -- ^ log TX to error log if it exceeds limits but accept TX
  | ErrorLimits    -- ^ reject TX if it exceeds the limits
  deriving (Show)

-- | Default slot config
defaultSlotConfig :: SlotConfig
defaultSlotConfig =
  SlotConfig
    { scSlotLength = 1000 -- each slot lasts for 1 second
    , scSlotZeroTime = 0 -- starts at unix epoch start
    }

defaultAlonzo :: MockConfig
defaultAlonzo = defaultMockConfig defaultAlonzoParams

defaultBabbage :: MockConfig
defaultBabbage = defaultMockConfig defaultBabbageParams

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
skipLimits cfg = cfg { mockConfigCheckLimits = IgnoreLimits }

-- | Warn on limits
warnLimits :: MockConfig -> MockConfig
warnLimits cfg = cfg { mockConfigCheckLimits = WarnLimits }

-- | Error on limits
forceLimits :: MockConfig -> MockConfig
forceLimits cfg = cfg { mockConfigCheckLimits = ErrorLimits }

{- | Read config for protocol parameters and form blockchain config.

 > readMockConfig protocolParametersFile
-}
readMockConfig :: FilePath -> IO MockConfig
readMockConfig paramsFile =
  defaultMockConfig {- TODO . setDefaultCostModel -} <$> readAlonzoParams paramsFile


