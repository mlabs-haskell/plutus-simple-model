module Cardano.Simple.PlutusLedgerApi.V1.Scripts (
  fromCompiledCode,
  mkValidatorScript,
  mkMintingPolicyScript,
  mkStakeValidatorScript,
  mkValidatorScriptPlutarch,
  mkMintingPolicyScriptPlutarch,
  mkStakeValidatorScriptPlutarch,
  mkPlutarchTypedScript,
  applyPlutarchTypedScript,
  Script (..),
  Validator (..),
  MintingPolicy (..),
  StakeValidator (..),
  ValidatorHash (..),
  StakeValidatorHash (..),
  MintingPolicyHash (..),
  PlutarchTypedScript (getPlutarchTypedScript),
) where

import Prelude qualified as Haskell

import Codec.CBOR.Decoding as CBOR
import Codec.Serialise (Serialise (..), serialise)
import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (throwError), when)
import Data.ByteString.Lazy qualified as BSL
import Data.Eq qualified as GHC
import Data.Text (Text)
import Flat qualified
import Flat.Decoder qualified as Flat
import GHC.Generics (Generic)
import Plutarch (ClosedTerm, Config, compile, (:-->))
import Plutarch.Script qualified as Plutarch
import PlutusCore qualified as PLC
import PlutusPrelude (over)
import PlutusTx (CompiledCode, getPlc)
import PlutusTx.Builtins as Builtins
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

-- | A script on the chain. This is an opaque type as far as the chain is concerned.
newtype Script = Script {unScript :: UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()}
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving (Serialise) via (SerialiseViaFlat (UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()))

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
    toNameless ::
      UPLC.Program UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () ->
      UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()
    toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn

mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
mkValidatorScript = Validator . fromCompiledCode

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy
mkMintingPolicyScript = MintingPolicy . fromCompiledCode

mkStakeValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> StakeValidator
mkStakeValidatorScript = StakeValidator . fromCompiledCode

mkValidatorScriptPlutarch :: Config -> ClosedTerm a -> Either Text Validator
mkValidatorScriptPlutarch conf term =
  Validator . Script . Plutarch.unScript <$> compile conf term

mkMintingPolicyScriptPlutarch :: Config -> ClosedTerm a -> Either Text MintingPolicy
mkMintingPolicyScriptPlutarch conf term =
  MintingPolicy . Script . Plutarch.unScript <$> compile conf term

mkStakeValidatorScriptPlutarch :: Config -> ClosedTerm a -> Either Text StakeValidator
mkStakeValidatorScriptPlutarch conf term =
  StakeValidator . Script . Plutarch.unScript <$> compile conf term

mkPlutarchTypedScript :: Config -> ClosedTerm a -> Either Text (PlutarchTypedScript a)
mkPlutarchTypedScript config term =
  PlutarchTypedScript . Script . Plutarch.unScript <$> Plutarch.compile config term

applyPlutarchTypedScript ::
  Config ->
  PlutarchTypedScript (a :--> b) ->
  ClosedTerm a ->
  Either Text (PlutarchTypedScript b)
applyPlutarchTypedScript
  config
  (PlutarchTypedScript (Script (UPLC.Program _ v1 t1)))
  term = do
    (Plutarch.Script (UPLC.Program _ v2 t2)) <- Plutarch.compile config term

    when (v1 GHC./= v2) $ throwError "Script versions differ"

    pure $
      PlutarchTypedScript $
        Script $
          UPLC.Program () v1 $
            UPLC.Apply () t1 t2

-- | 'PlutarchTypedScript' represents a compiled plutarch script while preserving type of the script
newtype PlutarchTypedScript s = PlutarchTypedScript {getPlutarchTypedScript :: Script}

-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator {getValidator :: Script}
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

instance Haskell.Show Validator where
  show = const "Validator { <script> }"

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy {getMintingPolicy :: Script}
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)

instance Haskell.Show MintingPolicy where
  show = const "MintingPolicy { <script> }"

-- | 'StakeValidator' is a wrapper around 'Script's which are used as validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator {getStakeValidator :: Script}
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

{- | Newtype to provide 'Serialise' instances for types with a 'Flat' instance that
 just encodes the flat-serialized value as a CBOR bytestring
-}
newtype SerialiseViaFlat a = SerialiseViaFlat a

instance Flat.Flat a => Serialise (SerialiseViaFlat a) where
  encode (SerialiseViaFlat a) = encode (Flat.flat a)
  decode = Haskell.fmap SerialiseViaFlat (decodeViaFlat Flat.decode)

decodeViaFlat :: Flat.Get a -> CBOR.Decoder s a
decodeViaFlat decoder = do
  bs <- decodeBytes
  -- lift any flat's failures to be cborg failures (MonadFail)
  Haskell.either (Haskell.fail . Haskell.show) Haskell.pure (Flat.unflatWith decoder bs)
