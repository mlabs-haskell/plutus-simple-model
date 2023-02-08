module Cardano.Simple.Eval (
  evalScript,
  utxoForTransaction,
  txBalance,
) where

import Prelude

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Records (HasField (getField))

import Cardano.Ledger.Alonzo.Data qualified as Ledger
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Era qualified as Ledger
import Cardano.Ledger.Language qualified as Ledger
import Cardano.Ledger.SafeHash qualified as SafeHash
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger

import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo

import Cardano.Simple.Cardano.Class (
  IsCardanoTx,
  getTxBody,
  toUtxo,
 )
import Cardano.Simple.Cardano.Common (ToCardanoError)
import Cardano.Simple.Ledger.Scripts qualified as C
import Cardano.Simple.Ledger.Tx (TxIn (TxIn), TxInType (ConsumePublicKeyAddress, ConsumeScriptAddress))
import PlutusLedgerApi.V2 (Address (Address), TxOut (TxOut), TxOutRef)

import Cardano.Simple.PlutusLedgerApi.V1.Scripts qualified as P
import PlutusLedgerApi.V2 qualified as P

import PlutusLedgerApi.Common qualified as Plutus

evalScript ::
  (HasField "_protocolVersion" (Ledger.PParams era) Ledger.ProtVer) =>
  Ledger.Language ->
  Ledger.PParams era ->
  Ledger.CostModel ->
  Plutus.SerialisedScript ->
  [Ledger.Data era] ->
  Maybe Plutus.EvaluationError
evalScript lang pparams cm script args =
  either Just (const Nothing) . snd $
    Plutus.evaluateScriptCounting
      (toPlutusLang lang)
      (Alonzo.transProtocolVersion $ getField @"_protocolVersion" pparams)
      Plutus.Verbose
      (Alonzo.getEvaluationContext cm)
      script
      (Ledger.getPlutusData <$> args)
  where
    toPlutusLang Ledger.PlutusV1 = Plutus.PlutusV1
    toPlutusLang Ledger.PlutusV2 = Plutus.PlutusV2

utxoForTransaction ::
  forall era.
  IsCardanoTx era =>
  Ledger.EraTxBody era =>
  Map P.ScriptHash (C.Versioned P.Script) ->
  Map TxOutRef TxOut ->
  Map P.DatumHash P.Datum ->
  Ledger.Network ->
  Ledger.Tx era ->
  Either ToCardanoError (Ledger.UTxO era)
utxoForTransaction scripts utxos datums network tx =
  toUtxo @era scripts network inOutList
  where
    inOutList :: [(TxIn, TxOut)]
    inOutList =
      [ (txin, out)
      | Ledger.TxIn txid ix <-
          Set.toList $
            Ledger.getAllTxInputs @era $
              getTxBody @era tx
      , let outRef =
              P.TxOutRef
                ( P.TxId $
                    P.toBuiltin $
                      SafeHash.originalBytes $
                        Ledger._unTxId txid
                )
                (toInteger $ Ledger.txIxToInt ix)
            out@(TxOut (Address cred _) _ od _) =
              fromMaybe (error "lookup failed") $
                Map.lookup outRef utxos
            txin = TxIn outRef $ case cred of
              P.PubKeyCredential _ -> pure ConsumePublicKeyAddress
              P.ScriptCredential sh ->
                pure $
                  ConsumeScriptAddress
                    ( pure $
                        fmap P.Validator $
                          fromMaybe (error "script lookup failed") $
                            Map.lookup sh scripts
                    )
                    (error "TODO how can I get the redeemer?")
                    ( case od of
                        P.OutputDatum d -> d
                        P.OutputDatumHash dh ->
                          fromMaybe (error "datum lookup failed") $
                            Map.lookup dh datums
                        P.NoOutputDatum -> error "Tx out had a script credential but no datum"
                        -- TODO make sure this should be an error
                    )
      ]

txBalance ::
  Ledger.PParams era ->
  Map P.ScriptHash (C.Versioned P.Script) ->
  Ledger.Network ->
  Ledger.Tx era ->
  Ledger.Value era
txBalance = error "TODO"
