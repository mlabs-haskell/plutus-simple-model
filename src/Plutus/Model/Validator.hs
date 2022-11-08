{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module Plutus.Model.Validator(
  IsData,
  HasDatum(..),
  HasRedeemer(..),
  HasLanguage(..),
  HasValidator(..),
  HasValidatorHash(..),

  IsValidator,
  IsValidatorHash,

  TypedValidator(..),
  TypedValidatorHash(..),
  TypedPolicy(..),
  TypedStake(..),
  -- * Versioned
  Versioned(..),
  toV1,
  toV2,
  isV1,
  isV2,
  toVersionedScript,

  -- * Hashes
  validatorHash,
  scriptHash,
  scriptCurrencySymbol,
  stakeValidatorHash,
  mintingPolicyHash,
  dataHash,
  datumHash,
  redeemerHash,
) where

import Cardano.Ledger.Alonzo.Language qualified as C
import Data.Coerce (coerce)
import Data.Kind (Type)
import Prelude

import Plutus.Model.Fork.Ledger.Scripts (Versioned (..), dataHash, datumHash, isV1, isV2, redeemerHash, toV1, toV2)
import Plutus.Model.Fork.Ledger.Scripts qualified as Fork
import Plutus.Model.Fork.TxExtra qualified as Fork
import Plutus.Model.Mock (
  AppendStaking (..),
  HasAddress (..),
  HasStakingCredential (..),
 )
import Plutus.Model.Fork.PlutusLedgerApi.V1.Scripts
import PlutusLedgerApi.V1

type IsData a = (ToData a, FromData a)

class IsData (DatumType a) => HasDatum a where
  type DatumType a :: Type

class IsData (RedeemerType a) => HasRedeemer a where
  type RedeemerType a :: Type

class HasLanguage a where
  getLanguage :: a -> C.Language
  -- ^ Get plutus language version

class HasValidator a where
  toValidator :: a -> Validator
  -- ^ Get internal avlidator

class HasValidatorHash a where
  toValidatorHash :: a -> ValidatorHash
  -- ^ Get internal avlidator

type IsValidator a = (HasAddress a, HasDatum a, HasRedeemer a, HasLanguage a, HasValidator a)
type IsValidatorHash a = (HasAddress a, HasDatum a, HasRedeemer a, HasLanguage a, HasValidatorHash a)

instance HasLanguage (Versioned a) where
  getLanguage (Versioned lang _) = lang

instance (HasLanguage a, HasValidator a) => HasValidatorHash a where
  toValidatorHash v = Fork.validatorHash $ Versioned (getLanguage v) (toValidator v)

---------------------------------------------------------------------
-- typed validator

-- | Typed validator. It's phantom type to annotate types for validators
newtype TypedValidator datum redeemer =
  TypedValidator { unTypedValidator :: Versioned Validator }
  deriving newtype (HasLanguage)

instance IsData datum => HasDatum (TypedValidator datum redeemer) where
  type DatumType (TypedValidator datum redeemer) = datum

instance IsData redeemer => HasRedeemer (TypedValidator datum redeemer) where
  type RedeemerType (TypedValidator datum redeemer) = redeemer

instance HasValidator (TypedValidator datum redeemer) where
  toValidator (TypedValidator (Versioned _lang validator)) = validator

instance HasAddress (TypedValidator datum redeemer) where
  toAddress = toAddress . toValidatorHash

---------------------------------------------------------------------
-- typed validator hash

-- | Typed validator. It's phantom type to annotate types for validators
newtype TypedValidatorHash datum redeemer =
  TypedValidatorHash { unTypedValidatorHash :: Versioned ValidatorHash }
  deriving newtype (HasLanguage)

instance IsData datum => HasDatum (TypedValidatorHash datum redeemer) where
  type DatumType (TypedValidatorHash datum redeemer) = datum

instance IsData redeemer => HasRedeemer (TypedValidatorHash datum redeemer) where
  type RedeemerType (TypedValidatorHash datum redeemer) = redeemer

instance HasValidatorHash (TypedValidatorHash datum redeemer) where
  toValidatorHash (TypedValidatorHash (Versioned _lang vh)) = vh

instance HasAddress (TypedValidatorHash datum redeemer) where
  toAddress (TypedValidatorHash (Versioned _lang vh)) = toAddress vh

---------------------------------------------------------------------
-- typed policy

-- | Typed minting policy. It's phantom type to annotate types for minting policies
newtype TypedPolicy redeemer =
  TypedPolicy { unTypedPolicy :: Versioned MintingPolicy }
  deriving newtype (HasLanguage)

instance IsData redeemer => HasRedeemer (TypedPolicy redeemer) where
  type RedeemerType (TypedPolicy redeemer) = redeemer

instance HasValidator (TypedPolicy redeemer) where
  toValidator (TypedPolicy (Versioned _lang (MintingPolicy script))) = Validator script

instance HasAddress (TypedPolicy redeemer) where
  toAddress = toAddress . toValidatorHash

---------------------------------------------------------------------
-- typed stake

-- | Typed stake valdiators. It's phantom type to annotate types for stake valdiators
newtype TypedStake redeemer =
  TypedStake { unTypedStake :: Versioned StakeValidator }
  deriving newtype (HasLanguage)

instance IsData redeemer => HasRedeemer (TypedStake redeemer) where
  type RedeemerType (TypedStake redeemer) = redeemer

instance HasValidator (TypedStake redeemer) where
  toValidator (TypedStake (Versioned _lang (StakeValidator script))) = Validator script

instance HasStakingCredential (TypedStake redeemer) where
  toStakingCredential (TypedStake script) = Fork.scriptToStaking script

instance HasAddress (TypedStake redeemer) where
  toAddress = toAddress . toValidatorHash

---------------------------------------------------------------------
-- append staking

instance {-# overlapping #-} IsData (DatumType a) => HasDatum (AppendStaking a) where
  type DatumType (AppendStaking a) = DatumType a

instance {-# overlapping #-} IsData (RedeemerType a) => HasRedeemer (AppendStaking a) where
  type RedeemerType (AppendStaking a) = RedeemerType a

instance {-# overlapping #-} HasLanguage a => HasLanguage (AppendStaking a) where
  getLanguage (AppendStaking _ a) = getLanguage a

instance {-# overlapping #-} HasValidator a => HasValidator (AppendStaking a) where
  toValidator (AppendStaking _ a) = toValidator a

---------------------------------------------------------------------
-- utils

-- | Converts typed validator to versioned script
toVersionedScript :: IsValidator a => a -> Versioned Script
toVersionedScript a = Versioned (getLanguage a) (getValidator $ toValidator a)

-- | Get valdiator hash
validatorHash :: (HasLanguage a, HasValidator a) => a -> ValidatorHash
validatorHash v = coerce $ Fork.validatorHash $ Versioned (getLanguage v) (toValidator v)

-- | Get script hash
scriptHash :: (HasLanguage a, HasValidator a) => a -> ScriptHash
scriptHash v = coerce $ Fork.validatorHash $ Versioned (getLanguage v) (toValidator v)

-- | Get currency symbol for minting policy
scriptCurrencySymbol :: TypedPolicy a -> CurrencySymbol
scriptCurrencySymbol (TypedPolicy script) = Fork.scriptCurrencySymbol script

-- | Get stake vlaidator hash
stakeValidatorHash :: TypedStake a -> StakeValidatorHash
stakeValidatorHash (TypedStake script) = Fork.stakeValidatorHash script

-- | Get minting policy hash
mintingPolicyHash :: TypedPolicy a -> MintingPolicyHash
mintingPolicyHash (TypedPolicy script) = Fork.mintingPolicyHash script

