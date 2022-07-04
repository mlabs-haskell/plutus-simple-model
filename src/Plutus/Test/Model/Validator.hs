{-# Language UndecidableInstances #-}
module Plutus.Test.Model.Validator(
  TypedValidator(..),
  TypedPolicy(..),
  IsValidator(..),
  mkTypedValidator,
  mkTypedPolicy,
) where

import Prelude
import Data.Kind (Type)

import PlutusTx.Code (CompiledCode)
import Plutus.V1.Ledger.Api
import Plutus.Test.Model.Blockchain (HasAddress(..), AppendStaking(..))

class (HasAddress script, ToData (DatumType script), FromData (DatumType script), ToData (RedeemerType script), FromData (RedeemerType script))
  => IsValidator script where
  type DatumType script    :: Type
  type RedeemerType script :: Type
  toValidator :: script -> Validator

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
