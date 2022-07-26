{-# Language UndecidableInstances #-}
module Plutus.Model.Validator(
  TypedValidator(..),
  TypedPolicy(..),
  TypedStake(..),
  IsValidator(..),
  Versioned(..),
  toVersionedScript,

  -- * Hashes
  validatorHash,
  scriptHash,
  scriptCurrencySymbol,
  stakeValidatorHash,
  mintingPolicyHash,
) where

import Prelude
import Data.Coerce (coerce)
import Data.Kind (Type)
import Cardano.Ledger.Alonzo.Language qualified as C

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (ScriptHash(..))
import Plutus.Model.Blockchain (
  HasAddress(..),
  AppendStaking(..),
  HasStakingCredential(..),
  )
import Plutus.Model.Fork.TxExtra qualified as Fork
import Plutus.Model.Fork.Ledger.Scripts (Versioned(..))
import Plutus.Model.Fork.Ledger.Scripts qualified as Fork

class (HasAddress script, ToData (DatumType script), FromData (DatumType script), ToData (RedeemerType script), FromData (RedeemerType script))
  => IsValidator script where
  type DatumType script    :: Type
  type RedeemerType script :: Type
  toValidator :: script -> Validator
  getLanguage :: script -> C.Language

instance (ToData datum, FromData datum, ToData redeemer, FromData redeemer)
  => IsValidator (TypedValidator datum redeemer) where
  type DatumType (TypedValidator datum redeemer) = datum
  type RedeemerType (TypedValidator datum redeemer) = redeemer
  toValidator (TypedValidator (Versioned _lang validator)) = validator
  getLanguage = versioned'language . unTypedValidator

instance (IsValidator script, ToData (DatumType script), FromData (DatumType script), ToData (RedeemerType script), FromData (RedeemerType script))
  => IsValidator (AppendStaking script) where
  type DatumType (AppendStaking script)    = DatumType script
  type RedeemerType (AppendStaking script) = RedeemerType script
  toValidator (AppendStaking _ script) = toValidator script
  getLanguage (AppendStaking _ script) = getLanguage script

instance (ToData redeemer, FromData redeemer) => IsValidator (TypedPolicy redeemer) where
  type DatumType (TypedPolicy redeemer) = ()
  type RedeemerType (TypedPolicy redeemer) = redeemer
  toValidator (TypedPolicy (Versioned _lang (MintingPolicy script))) = Validator script
  getLanguage = versioned'language . unTypedPolicy

toVersionedScript :: IsValidator a => a -> Versioned Script
toVersionedScript a = Versioned (getLanguage a) (getValidator $ toValidator a)

validatorHash :: IsValidator a => a -> ValidatorHash
validatorHash v = Fork.validatorHash $ Versioned (getLanguage v) (toValidator v)

scriptHash :: IsValidator a => a -> ScriptHash
scriptHash = coerce . validatorHash

-- | Phantom type to annotate types
newtype TypedValidator datum redeemer =
  TypedValidator { unTypedValidator :: (Versioned Validator) }

instance (ToData datum, ToData redeemer, FromData datum, FromData redeemer)
  => HasAddress (TypedValidator datum redeemer) where
  toAddress = toAddress . validatorHash

-- | Phantom type to annotate types
data TypedPolicy redeemer =
  TypedPolicy { unTypedPolicy :: Versioned MintingPolicy }

instance (ToData redeemer, FromData redeemer) => HasAddress (TypedPolicy redeemer) where
  toAddress = toAddress . validatorHash

newtype TypedStake redeemer =
  TypedStake { unTypedStake :: Versioned StakeValidator }

instance (ToData redeemer, FromData redeemer) => IsValidator (TypedStake redeemer) where
  type DatumType (TypedStake redeemer) = ()
  type RedeemerType (TypedStake redeemer) = redeemer
  toValidator (TypedStake (Versioned _lang (StakeValidator script))) = Validator script
  getLanguage = versioned'language . unTypedStake

instance (ToData redeemer, FromData redeemer) => HasAddress (TypedStake redeemer) where
  toAddress = toAddress . validatorHash

instance HasStakingCredential (TypedStake redeemer) where
  toStakingCredential (TypedStake script) = Fork.scriptToStaking script

---------------------------------------------------------------------------------

scriptCurrencySymbol :: TypedPolicy a -> CurrencySymbol
scriptCurrencySymbol (TypedPolicy script) = Fork.scriptCurrencySymbol script

mintingPolicyHash :: TypedPolicy a -> MintingPolicyHash
mintingPolicyHash (TypedPolicy script) = Fork.mintingPolicyHash script

stakeValidatorHash :: TypedStake a -> StakeValidatorHash
stakeValidatorHash (TypedStake script) = Fork.stakeValidatorHash script

---------------------------------------------------------------------------------

