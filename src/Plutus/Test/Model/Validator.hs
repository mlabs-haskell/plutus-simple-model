{-# Language UndecidableInstances #-}
module Plutus.Test.Model.Validator(
  TypedValidator(..),
  TypedPolicy(..),
  TypedStake(..),
  IsValidator(..),
  mkTypedValidator,
  mkTypedPolicy,
  mkTypedStake,
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

import PlutusTx.Code (CompiledCode)
import Plutus.V1.Ledger.Api
import Plutus.Test.Model.Blockchain (
  HasAddress(..),
  AppendStaking(..),
  HasStakingCredential(..),
  )
import PlutusTx.Prelude qualified as Plutus
import Plutus.Test.Model.Fork.Ledger.Scripts qualified as Fork

class (HasAddress script, ToData (DatumType script), FromData (DatumType script), ToData (RedeemerType script), FromData (RedeemerType script))
  => IsValidator script where
  type DatumType script    :: Type
  type RedeemerType script :: Type
  toValidator :: script -> Validator

instance IsValidator Validator where
  type DatumType Validator = BuiltinData
  type RedeemerType Validator = BuiltinData
  toValidator = id

instance (ToData datum, FromData datum, ToData redeemer, FromData redeemer)
  => IsValidator (TypedValidator datum redeemer) where
  type DatumType (TypedValidator datum redeemer) = datum
  type RedeemerType (TypedValidator datum redeemer) = redeemer
  toValidator (TypedValidator validator) = validator

instance (IsValidator script, ToData (DatumType script), FromData (DatumType script), ToData (RedeemerType script), FromData (RedeemerType script))
  => IsValidator (AppendStaking script) where
  type DatumType (AppendStaking script)    = DatumType script
  type RedeemerType (AppendStaking script) = RedeemerType script
  toValidator (AppendStaking _ script) = toValidator script

instance (ToData redeemer, FromData redeemer) => IsValidator (TypedPolicy redeemer) where
  type DatumType (TypedPolicy redeemer) = ()
  type RedeemerType (TypedPolicy redeemer) = redeemer
  toValidator (TypedPolicy (MintingPolicy script)) = Validator script

validatorHash :: IsValidator a => a -> ValidatorHash
validatorHash = Fork.validatorHash . toValidator

-- | Phantom type to annotate types
newtype TypedValidator datum redeemer = TypedValidator
  { unTypedValidator :: Validator }

instance HasAddress (TypedValidator datum redeemer) where
  toAddress = toAddress . unTypedValidator

-- | Phantom type to annotate types
newtype TypedPolicy redeemer = TypedPolicy
  { unTypedPolicy :: MintingPolicy }

instance (ToData redeemer, FromData redeemer) => HasAddress (TypedPolicy redeemer) where
  toAddress = toAddress . toValidator

mkTypedValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidator = TypedValidator . mkValidatorScript

mkTypedPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicy = TypedPolicy . mkMintingPolicyScript

mkTypedStake :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStake = TypedStake . mkStakeValidatorScript

newtype TypedStake redeemer = TypedStake { unTypedStake :: StakeValidator }

instance (ToData redeemer, FromData redeemer) => IsValidator (TypedStake redeemer) where
  type DatumType (TypedStake redeemer) = ()
  type RedeemerType (TypedStake redeemer) = redeemer
  toValidator (TypedStake (StakeValidator script)) = Validator script

instance (ToData redeemer, FromData redeemer) => HasAddress (TypedStake redeemer) where
  toAddress = toAddress . toValidator

instance HasStakingCredential (TypedStake redeemer) where
  toStakingCredential (TypedStake script) = toStakingCredential script

---------------------------------------------------------------------------------

scriptCurrencySymbol :: TypedPolicy a -> CurrencySymbol
scriptCurrencySymbol = Fork.scriptCurrencySymbol . unTypedPolicy

mintingPolicyHash :: TypedPolicy a -> MintingPolicyHash
mintingPolicyHash = Fork.mintingPolicyHash . unTypedPolicy

stakeValidatorHash :: TypedStake a -> StakeValidatorHash
stakeValidatorHash = Fork.stakeValidatorHash . unTypedStake

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

