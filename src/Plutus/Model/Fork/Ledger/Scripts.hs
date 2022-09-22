module Plutus.Model.Fork.Ledger.Scripts (
  Versioned(..),
  toV1,
  toV2,
  isV1,
  isV2,
  datumHash,
  dataHash,
  redeemerHash,
  validatorHash,
  stakeValidatorHash,
  scriptHash,
  mintingPolicyHash,
  scriptCurrencySymbol,
  toScript,
) where

import Prelude
import Data.Coerce
import Control.DeepSeq (NFData)
import GHC.Generics

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS

import Cardano.Ledger.Era qualified as C
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.SafeHash qualified as C
import Cardano.Ledger.Alonzo.Data qualified as C
import Cardano.Ledger.Alonzo qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as C
import Cardano.Ledger.Alonzo.Language qualified as C
import Cardano.Ledger.Alonzo.TxInfo qualified as C
import Cardano.Ledger.Mary.Value qualified as C
import PlutusLedgerApi.V2 qualified as P
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Alonzo (AlonzoEra)

-- | Appends plutus version to the value.
data Versioned a = Versioned
  { versioned'language :: !C.Language
  , versioned'content  :: a
  }
  deriving (Show, Eq, Ord, Generic, NFData, Functor)

-- | Version with PlutusV1
toV1 :: a -> Versioned a
toV1 = Versioned C.PlutusV1

-- | Version with PlutusV2
toV2 :: a -> Versioned a
toV2 = Versioned C.PlutusV2

-- | Check that version is PlutusV1
isV1 :: Versioned a -> Bool
isV1 x = case versioned'language x of
  C.PlutusV1 -> True
  _          -> False

-- | Check that version is PlutusV2
isV2 :: Versioned a -> Bool
isV2 x = case versioned'language x of
  C.PlutusV2 -> True
  _          -> False

datumHash :: P.Datum -> P.DatumHash
datumHash (P.Datum (P.BuiltinData dat)) =
  C.transDataHash' $ C.hashData $ C.Data @(AlonzoEra StandardCrypto) dat

redeemerHash :: P.Redeemer -> P.RedeemerHash
redeemerHash = coerce dataHash

dataHash :: P.BuiltinData -> P.BuiltinByteString
dataHash (P.BuiltinData dat) =
  P.toBuiltin . C.hashToBytes . C.extractHash $ C.hashData $ C.Data @(AlonzoEra StandardCrypto) dat

-- | Hash a 'PV1.Validator' script.
validatorHash :: Versioned P.Validator -> P.ValidatorHash
validatorHash val =
    C.transScriptHash
  $ C.hashScript @(AlonzoEra StandardCrypto)
  $ toScript (fmap coerce val)

stakeValidatorHash :: Versioned P.StakeValidator -> P.StakeValidatorHash
stakeValidatorHash = coerce validatorHash

scriptHash :: Versioned P.Script -> P.ScriptHash
scriptHash = coerce validatorHash

mintingPolicyHash :: Versioned P.MintingPolicy -> P.MintingPolicyHash
mintingPolicyHash = coerce validatorHash

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: Versioned P.MintingPolicy -> P.CurrencySymbol
scriptCurrencySymbol policy =
 C.transPolicyID
 $ C.PolicyID
 $ C.hashScript @(AlonzoEra StandardCrypto)
 $ toScript (fmap coerce policy)

toScript :: Versioned P.Script -> C.Script era
toScript (Versioned lang script) =
  C.PlutusScript lang $ SBS.toShort $ BSL.toStrict $ serialise script


