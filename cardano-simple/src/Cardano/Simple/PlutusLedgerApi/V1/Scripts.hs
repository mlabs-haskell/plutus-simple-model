module Cardano.Simple.PlutusLedgerApi.V1.Scripts (
  fromCompiledCode,
  mkValidatorScript,
  mkMintingPolicyScript,
  mkStakeValidatorScript,
  Script (..),
  Validator (..),
  MintingPolicy (..),
  StakeValidator (..),
  ValidatorHash (..),
  StakeValidatorHash (..),
  MintingPolicyHash (..),
) where

import Prelude qualified as Haskell

import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import PlutusTx (CompiledCode)
import PlutusTx.Builtins as Builtins
import PlutusTx.Prelude
import PlutusLedgerApi.Common (serialiseCompiledCode, SerialisedScript)

-- | A script on the chain. This is an opaque type as far as the chain is concerned.
newtype Script = Script {unScript :: SerialisedScript}
  deriving stock (Generic)
  deriving newtype (Haskell.Eq, Haskell.Ord)
  deriving anyclass (NFData)

instance Haskell.Show Script where
  showsPrec _ _ = Haskell.showString "<Script>"

-- | Turn a 'CompiledCode' (usually produced by 'compile') into a 'Script' for use with this package.
fromCompiledCode :: CompiledCode a -> Script
fromCompiledCode = Script . serialiseCompiledCode

mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
mkValidatorScript = Validator . fromCompiledCode

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy
mkMintingPolicyScript = MintingPolicy . fromCompiledCode

mkStakeValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> StakeValidator
mkStakeValidatorScript = StakeValidator . fromCompiledCode

-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator {getValidator :: Script}
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord)

instance Haskell.Show Validator where
  show = const "Validator { <script> }"

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy {getMintingPolicy :: Script}
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord)

instance Haskell.Show MintingPolicy where
  show = const "MintingPolicy { <script> }"

-- | 'StakeValidator' is a wrapper around 'Script's which are used as validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator {getStakeValidator :: Script}
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord)

instance Haskell.Show StakeValidator where
  show = const "StakeValidator { <script> }"

{- | Type representing the /BLAKE2b-224/ hash of a validator. 28 bytes.

This is a simple type without any validation, __use with caution__.
You may want to add checks for its invariants. See the
 [Shelley ledger specification](https://hydra.iohk.io/build/16861845/download/1/ledger-spec.pdf).
-}
newtype ValidatorHash
  = ValidatorHash Builtins.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

{- | Type representing the /BLAKE2b-224/ hash of a stake validator. 28 bytes.

This is a simple type without any validation, __use with caution__.
You may want to add checks for its invariants. See the
 [Shelley ledger specification](https://hydra.iohk.io/build/16861845/download/1/ledger-spec.pdf).
-}
newtype StakeValidatorHash
  = StakeValidatorHash Builtins.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

{- | Type representing the /BLAKE2b-224/ hash of a minting policy. 28 bytes.

This is a simple type without any validation, __use with caution__.
You may want to add checks for its invariants. See the
 [Shelley ledger specification](https://hydra.iohk.io/build/16861845/download/1/ledger-spec.pdf).
-}
newtype MintingPolicyHash
  = MintingPolicyHash Builtins.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)
