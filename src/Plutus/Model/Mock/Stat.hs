-- | Tx execution statistics (execution units budget)
module Plutus.Model.Mock.Stat(
  TxStat(..),
  txStatId,
  Stat(..),
  mainnetTxLimits,
  testnetTxLimits,
  mainnetBlockLimits,
  testnetBlockLimits,
  Percent(..),
  PercentExecutionUnits(..),
  StatPercent(..),
  toPercent,
  toStatPercent,
) where

import Prelude
import GHC.Natural
import Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import Plutus.V2.Ledger.Api
import Plutus.Model.Fork.Ledger.Tx qualified as P
import Plutus.Model.Fork.Ledger.Slot
import Plutus.Model.Fork.TxExtra
import Plutus.Model.Mock.Percent

-- | TX with stats of TX execution onchain.
data TxStat = TxStat
  { txStatTx        :: !Tx
  , txStatTime      :: !Slot
  , txStat          :: !Stat
  , txStatPercent   :: !StatPercent
  }
  deriving (Show)

-- | Gets Tx's hash
txStatId :: TxStat -> TxId
txStatId = P.txId . tx'plutus . txStatTx

-- | Stats of TX execution onchain.
data Stat = Stat
  { statSize           :: !Integer    -- ^ TX-size in bytes
  , statExecutionUnits :: !ExUnits    -- ^ execution units of TX
  }
  deriving (Show, Eq)

-- | Get Stats expressed in percents based on maximum limits and given stats.
toStatPercent :: Stat -> Stat -> StatPercent
toStatPercent maxStat stat =
  StatPercent
    { statPercentSize = percent statSize
    , statPercentExecutionUnits = PercentExecutionUnits
        { percentExecutionSteps  = percentNat (\(ExUnits _ steps) -> steps)
        , percentExecutionMemory = percentNat (\(ExUnits mem _)   -> mem)
        }
    }
  where
    percentNat getter = percent (naturalToInteger . getter . statExecutionUnits)

    percent :: (Stat -> Integer) -> Percent
    percent getter = toPercent (getter maxStat) (getter stat)

---------------------------------------------------------------------
-- stat resources limits (Alonzo era)

-- | Limits for TX-execution resources on Mainnet (Alonzo)
mainnetTxLimits :: Stat
mainnetTxLimits =
  Stat
    { statSize  = 16 * 1024
    , statExecutionUnits =
        let memory = 14_000_000
            steps  = 10_000_000_000
        in ExUnits memory steps
    }

-- | Limits for Block-execution resources resources on Mainnet
mainnetBlockLimits :: Stat
mainnetBlockLimits =
  Stat
    { statSize = 65 * 1024
    , statExecutionUnits =
        let memory = 50_000_000
            steps = 40_000_000_000
        in ExUnits memory steps
    }

-- | Limits for TX-execution resources resources on Testnet
testnetTxLimits :: Stat
testnetTxLimits = mainnetTxLimits

-- | Limits for Block-execution resources resources on Testnet
testnetBlockLimits :: Stat
testnetBlockLimits = mainnetBlockLimits

