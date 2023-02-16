-- | Creation of typed validators for Plutus V1
module Plutus.Model.Validator.V1 (
  mkTypedValidator,
  mkTypedPolicy,
  mkTypedStake,
  mkTypedValidatorPlutarch,
  mkTypedPolicyPlutarch,
  mkTypedStakePlutarch,
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
import Plutarch.Api.V1 (PMintingPolicy, PStakeValidator, PValidator)
import Plutus.Model.Validator (TypedPolicy (..), TypedStake (..), TypedValidator (..))

import Data.Text (Text)
import Plutarch (ClosedTerm, Config)
import Prelude (Either, (<$>))

-- | Create Plutus V1 typed validator
mkTypedValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidator = TypedValidator . toV1 . mkValidatorScript

-- | Create Plutus V1 typed minting policy
mkTypedPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicy = TypedPolicy . toV1 . mkMintingPolicyScript

-- | Create Plutus V1 typed stake validator
mkTypedStake :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStake = TypedStake . toV1 . mkStakeValidatorScript

-- | Create Plutus V1 typed validator from a plutarch term
mkTypedValidatorPlutarch :: Config -> ClosedTerm PValidator -> Either Text (TypedValidator datum redeemer)
mkTypedValidatorPlutarch conf term = TypedValidator . toV1 <$> mkValidatorScriptPlutarch conf term

-- | Create Plutus V1 typed minting policy from a plutarch term
mkTypedPolicyPlutarch :: Config -> ClosedTerm PMintingPolicy -> Either Text (TypedPolicy redeemer)
mkTypedPolicyPlutarch conf term = TypedPolicy . toV1 <$> mkMintingPolicyScriptPlutarch conf term

-- | Create Plutus V1 typed stake validator from a plutarch term
mkTypedStakePlutarch :: Config -> ClosedTerm PStakeValidator -> Either Text (TypedStake redeemer)
mkTypedStakePlutarch conf term = TypedStake . toV1 <$> mkStakeValidatorScriptPlutarch conf term

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
