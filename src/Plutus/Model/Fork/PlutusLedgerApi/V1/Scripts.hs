module Plutus.Model.Fork.PlutusLedgerApi.V1.Scripts (
  fromCompiledCode,
  mkMintingPolicyScript,
  Script (..),
  Validator (..),
  MintingPolicy (..),
  StakeValidator (..),
  ValidatorHash (..),
  StakeValidatorHash (..),
  MintingPolicyHash (..),
) where

import Prelude qualified as Haskell

import Codec.CBOR.Decoding as CBOR
import Codec.Serialise ( Serialise(..), serialise )
import Control.DeepSeq ( NFData )
import Data.ByteString.Lazy qualified as BSL
import Flat qualified
import Flat.Decoder qualified as Flat
import GHC.Generics(Generic)
import PlutusPrelude (over)
import PlutusTx (CompiledCode, getPlc)
import PlutusTx.Builtins as Builtins
import PlutusTx.Prelude
import PlutusCore qualified as PLC
import UntypedPlutusCore qualified as UPLC

-- | A script on the chain. This is an opaque type as far as the chain is concerned.
newtype Script = Script { unScript :: UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun () }
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving Serialise via (SerialiseViaFlat (UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()))

instance Haskell.Eq Script where
    a == b = Builtins.toBuiltin (BSL.toStrict (serialise a)) == Builtins.toBuiltin (BSL.toStrict (serialise b))
instance Haskell.Ord Script where
    a `compare` b = Builtins.toBuiltin (BSL.toStrict (serialise a)) `compare` Builtins.toBuiltin (BSL.toStrict (serialise b))

instance Haskell.Show Script where
    showsPrec _ _ = Haskell.showString "<Script>"

-- | Turn a 'CompiledCode' (usually produced by 'compile') into a 'Script' for use with this package.
fromCompiledCode :: CompiledCode a -> Script
fromCompiledCode = Script . toNameless . getPlc
    where
      toNameless :: UPLC.Program UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
                 -> UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()
      toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy
mkMintingPolicyScript = MintingPolicy . fromCompiledCode

-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator { getValidator :: Script }
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

instance Haskell.Show Validator where
    show = const "Validator { <script> }"

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy { getMintingPolicy :: Script }
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

instance Haskell.Show MintingPolicy where
    show = const "MintingPolicy { <script> }"

-- | 'StakeValidator' is a wrapper around 'Script's which are used as validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator { getStakeValidator :: Script }
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

instance Haskell.Show StakeValidator where
    show = const "StakeValidator { <script> }"

{- | Type representing the /BLAKE2b-224/ hash of a validator. 28 bytes.

This is a simple type without any validation, __use with caution__.
You may want to add checks for its invariants. See the
 [Shelley ledger specification](https://hydra.iohk.io/build/16861845/download/1/ledger-spec.pdf).
-}
newtype ValidatorHash =
    ValidatorHash Builtins.BuiltinByteString
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

{- | Type representing the /BLAKE2b-224/ hash of a stake validator. 28 bytes.

This is a simple type without any validation, __use with caution__.
You may want to add checks for its invariants. See the
 [Shelley ledger specification](https://hydra.iohk.io/build/16861845/download/1/ledger-spec.pdf).
-}
newtype StakeValidatorHash =
    StakeValidatorHash Builtins.BuiltinByteString
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

{- | Type representing the /BLAKE2b-224/ hash of a minting policy. 28 bytes.

This is a simple type without any validation, __use with caution__.
You may want to add checks for its invariants. See the
 [Shelley ledger specification](https://hydra.iohk.io/build/16861845/download/1/ledger-spec.pdf).
-}
newtype MintingPolicyHash =
    MintingPolicyHash Builtins.BuiltinByteString
    deriving stock (Generic)
    deriving anyclass (NFData)
    deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

-- | Newtype to provide 'Serialise' instances for types with a 'Flat' instance that
-- just encodes the flat-serialized value as a CBOR bytestring
newtype SerialiseViaFlat a = SerialiseViaFlat a
instance Flat.Flat a => Serialise (SerialiseViaFlat a) where
  encode (SerialiseViaFlat a) = encode (Flat.flat a)
  decode = Haskell.fmap SerialiseViaFlat (decodeViaFlat Flat.decode)

decodeViaFlat :: Flat.Get a -> CBOR.Decoder s a
decodeViaFlat decoder = do
    bs <- decodeBytes
    -- lift any flat's failures to be cborg failures (MonadFail)
    Haskell.either (\x -> Haskell.fail (Haskell.show x)) Haskell.pure (Flat.unflatWith decoder bs)
