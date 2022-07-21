module Plutus.Test.Model.Blockchain.BchConfig(
  Stat(..),
  BchConfig(..),
  CheckLimits(..),
  defaultSlotConfig,
  defaultBchConfig,
  defaultAlonzo,
  defaultBabbage,
  defaultAlonzoParams,
  defaultBabbageParams,
  skipLimits,
  warnLimits,
  forceLimits,
  readBchConfig,
) where

import Prelude
import Cardano.Ledger.BaseTypes
import Plutus.Test.Model.Fork.Ledger.TimeSlot (SlotConfig (..))
import Plutus.Test.Model.Blockchain.ProtocolParameters
import Plutus.Test.Model.Blockchain.Stat

-- | Config for the blockchain.
data BchConfig = BchConfig
  { bchConfigCheckLimits  :: !CheckLimits       -- ^ limits check mode
  , bchConfigLimitStats   :: !Stat              -- ^ TX execution resources limits
  , bchConfigProtocol     :: !PParams           -- ^ Protocol parameters
  , bchConfigNetworkId    :: !Network           -- ^ Network id (mainnet / testnet)
  , bchConfigSlotConfig   :: !SlotConfig        -- ^ Slot config
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

defaultAlonzo :: BchConfig
defaultAlonzo = defaultBchConfig defaultAlonzoParams

defaultBabbage :: BchConfig
defaultBabbage = defaultBchConfig defaultBabbageParams

-- | Default blockchain config.
defaultBchConfig :: PParams -> BchConfig
defaultBchConfig params =
  BchConfig
    { bchConfigLimitStats = mainnetTxLimits
    , bchConfigCheckLimits = ErrorLimits
    , bchConfigProtocol = params
    , bchConfigNetworkId = Mainnet
    , bchConfigSlotConfig = defaultSlotConfig
    }

-- | Do not check for limits
skipLimits :: BchConfig -> BchConfig
skipLimits cfg = cfg { bchConfigCheckLimits = IgnoreLimits }

-- | Warn on limits
warnLimits :: BchConfig -> BchConfig
warnLimits cfg = cfg { bchConfigCheckLimits = WarnLimits }

-- | Error on limits
forceLimits :: BchConfig -> BchConfig
forceLimits cfg = cfg { bchConfigCheckLimits = ErrorLimits }

{- | Read config for protocol parameters and form blockchain config.

 > readBchConfig protocolParametersFile
-}
readBchConfig :: FilePath -> IO BchConfig
readBchConfig paramsFile =
  defaultBchConfig {- TODO . setDefaultCostModel -} <$> readAlonzoParams paramsFile


