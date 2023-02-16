-- | Creation of typed validators for Plutus V2
module Plutus.Model.Validator.V2 (
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

import PlutusLedgerApi.V2
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (Bool, (.))
import PlutusTx.Prelude qualified as Plutus

import Cardano.Simple.Ledger.Scripts (toV2)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts
import Plutus.Model.Validator (TypedPolicy (..), TypedStake (..), TypedValidator (..))

import Data.Text (Text)
import Plutarch (ClosedTerm, Config)
import Prelude (Either(..), (<$>), ($))

import Debug.Trace

-- | Create Plutus V2 typed validator
mkTypedValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> TypedValidator datum redeemer
mkTypedValidator = TypedValidator . toV2 . mkValidatorScript

-- | Create Plutus V2 typed minting policy
mkTypedPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedPolicy = TypedPolicy . toV2 . mkMintingPolicyScript

-- | Create Plutus V2 typed stake validator
mkTypedStake :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer
mkTypedStake = TypedStake . toV2 . mkStakeValidatorScript

-- | Create Plutus V2 typed validator from a plutarch term
mkTypedValidatorPlutarch :: Config -> ClosedTerm p -> Either Text (TypedValidator datum redeemer)
mkTypedValidatorPlutarch conf term = TypedValidator . toV2 <$> mkValidatorScriptPlutarch conf term

-- | Create Plutus V2 typed minting policy from a plutarch term
mkTypedPolicyPlutarch :: Config -> ClosedTerm p -> Either Text (TypedPolicy redeemer)
mkTypedPolicyPlutarch conf term = do
  case mkMintingPolicyScriptPlutarch conf term of
    Right res -> do
      traceM "RRRRRRRR: mkTypedPolicyPlutarch"
      case toV2 <$> Right res of
        Right r -> do
          traceM "----------- RRRRRRrrrrrr"
          Right $ TypedPolicy r
        Left e -> do
          traceM "----------- LLLLLLLLLlllll"
          Left e
    Left err -> do
      traceM "LLLLLLLL: mkTypedPolicyPlutarch"
      TypedPolicy . toV2 <$> Left err

-- | Create Plutus V2 typed stake validator from a plutarch term
mkTypedStakePlutarch :: Config -> ClosedTerm p -> Either Text (TypedStake redeemer)
mkTypedStakePlutarch conf term = TypedStake . toV2 <$> mkStakeValidatorScriptPlutarch conf term

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
