{-# Language UndecidableInstances #-}
module Plutus.Test.Model.Validator(
  TypedValidator(..),
  TypedPolicy(..),
  IsValidator(..),
) where

import Prelude
import Data.Kind (Type)

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

-- | Phantom type to annotate types
newtype TypedValidator datum redeemer = TypedValidator
  { unTypedValidator :: Validator }

instance HasAddress ( TypedValidator datum redeemer) where
  toAddress = toAddress . unTypedValidator

-- | Phantom type to annotate types
newtype TypedPolicy redeemer = TypedPolicy
  { unTypedPolicy :: MintingPolicy }




