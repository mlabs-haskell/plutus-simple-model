{-# Language UndecidableInstances #-}
module Plutus.Test.Model.Validator(
  TypedValidator(..),
  TypedPolicy(..),
  TypedStake(..),
  IsValidator(..),
  mkTypedValidatorV1,
  mkTypedPolicyV1,
  mkTypedStakeV1,
  mkTypedValidatorV2,
  mkTypedPolicyV2,
  mkTypedStakeV2,
  -- utils
  toBuiltinValidator,
  toBuiltinPolicy,
  toBuiltinStake,

  -- * Hashes
  validatorHash,
  scriptCurrencySymbol,
  stakeValidatorHash,
  mintingPolicyHash,
) where

import Prelude
import Data.Kind (Type)
import Cardano.Ledger.Alonzo.Language qualified as C

import PlutusTx.Code (CompiledCode)
import Plutus.V1.Ledger.Api
import Plutus.Test.Model.Blockchain (
  HasAddress(..),
  AppendStaking(..),
  HasStakingCredential(..),
  )
import PlutusTx.Prelude qualified as Plutus
import Plutus.Test.Model.Fork.TxExtra qualified as Fork
import Plutus.Test.Model.Fork.Ledger.Scripts qualified as Fork

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
  toValidator (TypedValidator _lang validator) = validator
  getLanguage = typedValidator'language

instance (IsValidator script, ToData (DatumType script), FromData (DatumType script), ToData (RedeemerType script), FromData (RedeemerType script))
  => IsValidator (AppendStaking script) where
  type DatumType (AppendStaking script)    = DatumType script
  type RedeemerType (AppendStaking script) = RedeemerType script
  toValidator (AppendStaking _ script) = toValidator script
  getLanguage (AppendStaking _ script) = getLanguage script

instance (ToData redeemer, FromData redeemer) => IsValidator (TypedPolicy redeemer) where
  type DatumType (TypedPolicy redeemer) = ()
  type RedeemerType (TypedPolicy redeemer) = redeemer
  toValidator (TypedPolicy _lang (MintingPolicy script)) = Validator script
  getLanguage = typedPolicy'language

validatorHash :: IsValidator a => a -> ValidatorHash
validatorHash v = Fork.validatorHash (getLanguage v) (toValidator v)

-- | Phantom type to annotate types
data TypedValidator datum redeemer = TypedValidator
  { typedValidator'language  :: C.Language
  , typedValidator'validator :: Validator
  }

instance (ToData datum, ToData redeemer, FromData datum, FromData redeemer)
  => HasAddress (TypedValidator datum redeemer) where
  toAddress = toAddress . validatorHash

-- | Phantom type to annotate types
data TypedPolicy redeemer = TypedPolicy
  { typedPolicy'language  :: C.Language
  , typedPolicy'validator :: MintingPolicy
  }

instance (ToData redeemer, FromData redeemer) => HasAddress (TypedPolicy redeemer) where
  toAddress = toAddress . validatorHash

mkTypedValidatorV1 :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidatorV1 = TypedValidator C.PlutusV1 . mkValidatorScript

mkTypedValidatorV2 :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidatorV2 = TypedValidator C.PlutusV2 . mkValidatorScript

mkTypedPolicyV1 :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicyV1 = TypedPolicy C.PlutusV1 . mkMintingPolicyScript

mkTypedPolicyV2 :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicyV2 = TypedPolicy C.PlutusV2 . mkMintingPolicyScript

mkTypedStakeV1 :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStakeV1 = TypedStake C.PlutusV1 . mkStakeValidatorScript

mkTypedStakeV2 :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStakeV2 = TypedStake C.PlutusV2 . mkStakeValidatorScript

data TypedStake redeemer = TypedStake
  { typedStake'language  :: C.Language
  , typedStake'validator :: StakeValidator
  }

instance (ToData redeemer, FromData redeemer) => IsValidator (TypedStake redeemer) where
  type DatumType (TypedStake redeemer) = ()
  type RedeemerType (TypedStake redeemer) = redeemer
  toValidator (TypedStake _lang (StakeValidator script)) = Validator script
  getLanguage = typedStake'language

instance (ToData redeemer, FromData redeemer) => HasAddress (TypedStake redeemer) where
  toAddress = toAddress . validatorHash

instance HasStakingCredential (TypedStake redeemer) where
  toStakingCredential (TypedStake lang script) = Fork.scriptToStaking lang script

---------------------------------------------------------------------------------

scriptCurrencySymbol :: TypedPolicy a -> CurrencySymbol
scriptCurrencySymbol (TypedPolicy lang script) = Fork.scriptCurrencySymbol lang script

mintingPolicyHash :: TypedPolicy a -> MintingPolicyHash
mintingPolicyHash (TypedPolicy lang script) = Fork.mintingPolicyHash lang script

stakeValidatorHash :: TypedStake a -> StakeValidatorHash
stakeValidatorHash (TypedStake lang script) = Fork.stakeValidatorHash lang script

---------------------------------------------------------------------------------

-- | Coverts to low-level validator representation
{-# INLINABLE toBuiltinValidator #-}
toBuiltinValidator :: (UnsafeFromData datum, UnsafeFromData redeemer)
  => (datum -> redeemer -> ScriptContext -> Bool) -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
toBuiltinValidator script datum act ctx =
  Plutus.check (
    script (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData act)
            (unsafeFromBuiltinData ctx))


-- | Coverts to low-level validator representation
{-# INLINABLE toBuiltinPolicy #-}
toBuiltinPolicy :: (UnsafeFromData redeemer)
  => (redeemer -> ScriptContext -> Bool) -> (BuiltinData -> BuiltinData -> ())
toBuiltinPolicy script act ctx =
  Plutus.check (
    script (unsafeFromBuiltinData act)
            (unsafeFromBuiltinData ctx))

-- | Coverts to low-level validator representation
{-# INLINABLE toBuiltinStake #-}
toBuiltinStake :: (UnsafeFromData redeemer)
  => (redeemer -> ScriptContext -> Bool) -> (BuiltinData -> BuiltinData -> ())
toBuiltinStake script act ctx =
  Plutus.check (
    script (unsafeFromBuiltinData act)
            (unsafeFromBuiltinData ctx))

