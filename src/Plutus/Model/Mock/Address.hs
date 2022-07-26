-- | Classes to get addresses and work with addresses
module Plutus.Model.Mock.Address(
  HasAddress(..),
  HasStakingCredential(..),
  AppendStaking(..),
  appendStakingCredential,
  appendStakingPubKey,
  appendStakingScript,
) where

import Prelude
import Data.Coerce
import Plutus.V1.Ledger.Address
import Plutus.V2.Ledger.Api
import Plutus.Model.Fork.TxExtra(keyToStaking)

-- | Everything that has address
class HasAddress a where
  toAddress :: a -> Address

instance HasAddress Address where
  toAddress = id

instance HasAddress PubKeyHash where
  toAddress = pubKeyHashAddress

instance HasAddress ValidatorHash where
  toAddress = scriptHashAddress

-- | Everything that has staking credential
class HasStakingCredential a where
  toStakingCredential :: a -> StakingCredential

instance HasStakingCredential StakingCredential where
  toStakingCredential = id

instance HasStakingCredential PubKeyHash where
  toStakingCredential = keyToStaking

-- | Encodes appening of staking address
data AppendStaking a =
  AppendStaking StakingCredential a

instance HasAddress a => HasAddress (AppendStaking a) where
  toAddress (AppendStaking stakeCred a) = appendStake (toAddress a)
    where
      appendStake addr = addr { addressStakingCredential = Just stakeCred }

-- | Append staking credential info
appendStakingCredential :: Credential -> a -> AppendStaking a
appendStakingCredential cred = AppendStaking (StakingHash cred)

-- | Append staking public key info
appendStakingPubKey :: PubKeyHash -> a -> AppendStaking a
appendStakingPubKey pkh = appendStakingCredential (PubKeyCredential pkh)

-- | Append staking script info
appendStakingScript :: StakeValidatorHash -> a -> AppendStaking a
appendStakingScript sh = appendStakingCredential (ScriptCredential $ coerce sh)

