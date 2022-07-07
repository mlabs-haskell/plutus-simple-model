module Plutus.Test.Model.Fork.Ledger.Scripts
    (
    datumHash,
    validatorHash,
    scriptCurrencySymbol,
    ) where

import Prelude

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Plutus.V1.Ledger.Api qualified as PV1

import Data.Proxy
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.SafeHash qualified as C
import Cardano.Ledger.Alonzo.Data qualified as C
import Cardano.Ledger.Alonzo qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as C
import Cardano.Ledger.Alonzo.Language qualified as C
import Cardano.Ledger.Alonzo.TxInfo qualified as C
import Cardano.Ledger.Hashes qualified as C
import Plutus.V1.Ledger.Api qualified as P
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Alonzo (AlonzoEra)

datumHash :: PV1.Datum -> PV1.DatumHash
datumHash (P.Datum (P.BuiltinData dat)) =
  C.transDataHash' $ C.hashData $ C.Data @(AlonzoEra StandardCrypto) dat

-- | Hash a 'PV1.Validator' script.
validatorHash :: PV1.Validator -> PV1.ValidatorHash
validatorHash (P.Validator script) =
  P.ValidatorHash
  $ scriptHash
  $ toScript C.PlutusV1 script

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: PV1.MintingPolicy -> PV1.CurrencySymbol
scriptCurrencySymbol (P.MintingPolicy script) =
  P.CurrencySymbol $ scriptHash $ toScript C.PlutusV1 script

toScript :: C.Language -> P.Script -> C.Script (AlonzoEra StandardCrypto)
toScript lang = C.PlutusScript lang . SBS.toShort . BSL.toStrict . serialise

scriptHash :: C.Script (AlonzoEra StandardCrypto) -> P.BuiltinByteString
scriptHash =
    P.toBuiltin
  . C.hashToBytes
  . C.extractHash
  . C.makeHashWithExplicitProxys (Proxy @StandardCrypto) (Proxy @C.EraIndependentScript)

