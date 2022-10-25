-- | Creation of typed validators for Plutus V2
module Plutus.Model.Validator.V2(
  mkTypedValidator,
  mkUntypedValidator,
  mkTypedPolicy,
  mkTypedStake,
  toBuiltinValidator,
  toBuiltinPolicy,
  toBuiltinStake,
) where

import PlutusTx.Prelude (Bool, (.))
import PlutusTx.Prelude qualified as Plutus
import Plutus.V2.Ledger.Api
import PlutusTx.Code (CompiledCode)

import Plutus.Model.Validator (TypedValidator(..), TypedPolicy(..), TypedStake(..), UntypedValidator (UntypedValidator))
import Plutus.Model.Fork.Ledger.Scripts (toV2)

-- | Create Plutus V2 typed validator
mkTypedValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidator = TypedValidator . toV2 . mkValidatorScript

-- | Create Plutus V2 untyped validator
mkUntypedValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> UntypedValidator
mkUntypedValidator = UntypedValidator . toV2 . mkValidatorScript

-- | Create Plutus V2 typed minting policy
mkTypedPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicy = TypedPolicy . toV2 . mkMintingPolicyScript

-- | Create Plutus V2 typed stake validator
mkTypedStake :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStake = TypedStake . toV2 . mkStakeValidatorScript

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

