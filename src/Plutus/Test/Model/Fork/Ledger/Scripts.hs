module Plutus.Test.Model.Fork.Ledger.Scripts (
  datumHash,
  validatorHash,
  scriptCurrencySymbol,
  toScript,
) where

import Prelude

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS

import Data.Proxy
import Cardano.Ledger.Era qualified as C
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.SafeHash qualified as C
import Cardano.Ledger.Alonzo.Data qualified as C
import Cardano.Ledger.Alonzo qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as C
import Cardano.Ledger.Alonzo.Language qualified as C
import Cardano.Ledger.Alonzo.TxInfo qualified as C
import Cardano.Ledger.Hashes qualified as C
import Cardano.Ledger.Mary.Value qualified as C
import Plutus.V1.Ledger.Api qualified as P
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Alonzo (AlonzoEra)

datumHash :: P.Datum -> P.DatumHash
datumHash (P.Datum (P.BuiltinData dat)) =
  C.transDataHash' $ C.hashData $ C.Data @(AlonzoEra StandardCrypto) dat

-- | Hash a 'PV1.Validator' script.
validatorHash :: P.Validator -> P.ValidatorHash
validatorHash (P.Validator script) =
    C.transScriptHash
  $ C.hashScript @(AlonzoEra StandardCrypto)
  $ toScript C.PlutusV1 script

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: P.MintingPolicy -> P.CurrencySymbol
scriptCurrencySymbol (P.MintingPolicy script) =
 C.transPolicyID
 $ C.PolicyID
 $ C.hashScript @(AlonzoEra StandardCrypto)
 $ toScript C.PlutusV1 script

toScript :: C.Language -> P.Script -> C.Script (AlonzoEra StandardCrypto)
toScript lang = C.PlutusScript lang . SBS.toShort . BSL.toStrict . serialise
