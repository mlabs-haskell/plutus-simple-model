module Plutus.Test.Model.Blockchain.Address(
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
import Plutus.V1.Ledger.Api
import Plutus.Test.Model.Fork.TxExtra(keyToStaking, scriptToStaking)
import Plutus.Test.Model.Fork.Ledger.Scripts (validatorHash)

class HasAddress a where
  toAddress :: a -> Address

instance HasAddress Address where
  toAddress = id

instance HasAddress PubKeyHash where
  toAddress = pubKeyHashAddress

instance HasAddress ValidatorHash where
  toAddress = scriptHashAddress

instance HasAddress Validator where
  toAddress = toAddress . validatorHash

class HasStakingCredential a where
  toStakingCredential :: a -> StakingCredential

instance HasStakingCredential StakingCredential where
  toStakingCredential = id

instance HasStakingCredential PubKeyHash where
  toStakingCredential = keyToStaking

instance HasStakingCredential StakeValidator where
  toStakingCredential = scriptToStaking

-- | Encodes appening of staking address
data AppendStaking a =
  AppendStaking StakingCredential a

instance HasAddress a => HasAddress (AppendStaking a) where
  toAddress (AppendStaking stakeCred a) = appendStake (toAddress a)
    where
      appendStake addr = addr { addressStakingCredential = Just stakeCred }

appendStakingCredential :: Credential -> a -> AppendStaking a
appendStakingCredential cred = AppendStaking (StakingHash cred)

appendStakingPubKey :: PubKeyHash -> a -> AppendStaking a
appendStakingPubKey pkh = appendStakingCredential (PubKeyCredential pkh)

appendStakingScript :: StakeValidatorHash -> a -> AppendStaking a
appendStakingScript sh = appendStakingCredential (ScriptCredential $ coerce sh)



