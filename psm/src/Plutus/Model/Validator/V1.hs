-- | Creation of typed validators for Plutus V1
module Plutus.Model.Validator.V1 (
  mkTypedValidator,
  mkTypedPolicy,
  mkTypedStake,
  toBuiltinValidator,
  toBuiltinPolicy,
  toBuiltinStake,
) where

import PlutusLedgerApi.V1
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (Bool, (.))
import PlutusTx.Prelude qualified as Plutus

import Cardano.Simple.Ledger.Scripts (toV1)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts
import Plutus.Model.Validator (TypedPolicy (..), TypedStake (..), TypedValidator (..))

-- | Create Plutus V1 typed validator
mkTypedValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidator = TypedValidator . toV1 . mkValidatorScript

-- | Create Plutus V1 typed minting policy
mkTypedPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicy = TypedPolicy . toV1 . mkMintingPolicyScript

-- | Create Plutus V1 typed stake validator
mkTypedStake :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStake = TypedStake . toV1 . mkStakeValidatorScript

-- | Coverts to low-level validator representation
{-# INLINEABLE toBuiltinValidator #-}
toBuiltinValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
toBuiltinValidator script datum act ctx =
  Plutus.check
    ( script
        (unsafeFromBuiltinData datum)
        (unsafeFromBuiltinData act)
        (unsafeFromBuiltinData ctx)
    )

-- | Coverts to low-level validator representation
{-# INLINEABLE toBuiltinPolicy #-}
toBuiltinPolicy ::
  (UnsafeFromData redeemer) =>
  (redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
toBuiltinPolicy script act ctx =
  Plutus.check
    ( script
        (unsafeFromBuiltinData act)
        (unsafeFromBuiltinData ctx)
    )

-- | Coverts to low-level validator representation
{-# INLINEABLE toBuiltinStake #-}
toBuiltinStake ::
  (UnsafeFromData redeemer) =>
  (redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
toBuiltinStake script act ctx =
  Plutus.check
    ( script
        (unsafeFromBuiltinData act)
        (unsafeFromBuiltinData ctx)
    )
