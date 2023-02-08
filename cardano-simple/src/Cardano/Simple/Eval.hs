module Cardano.Simple.Eval (
  evalScript,
  utxoForTransaction,
  txBalance,
  evaluateScriptsInTx,
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
import Cardano.Ledger.Language qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger

import Cardano.Ledger.Alonzo.Tools (evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, TranslationError)
import Cardano.Ledger.Shelley.API (CLI, evaluateTransactionBalance)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody)

import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Simple.TxExtra qualified as P

import Cardano.Simple.Cardano.Class (
  IsCardanoTx,
  getTxBody,
  toCardanoTx',
  toUtxo,
 )
import Cardano.Simple.Cardano.Common (ToCardanoError)
import Cardano.Simple.Ledger.Tx (
  Tx (txCollateral, txInputs, txReferenceInputs, txScripts),
  TxIn (TxIn),
 )
import PlutusLedgerApi.V2 (TxOut, TxOutRef)

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
  Map TxOutRef TxOut ->
  Ledger.Network ->
  Tx ->
  Either ToCardanoError (Ledger.UTxO era)
utxoForTransaction utxos network tx =
  toUtxo @era (txScripts tx) network inOutList
  where
    inOutList :: [(TxIn, TxOut)]
    inOutList =
      [ (txin, out)
      | txin@(TxIn outRef _) <-
          Set.toList $
            txInputs tx
              <> txCollateral tx
              <> txReferenceInputs tx
      , let out =
              fromMaybe (error "lookup failed") $
                Map.lookup outRef utxos
      ]

txBalance ::
  forall era.
  ( IsCardanoTx era
  , CLI era
  , ShelleyEraTxBody era
  ) =>
  Map TxOutRef TxOut ->
  Ledger.PParams era ->
  Ledger.Network ->
  Tx ->
  P.Extra ->
  Either ToCardanoError (Ledger.Value era)
txBalance utxos pparams network tx extra = do
  utxo <- utxoForTransaction @era utxos network tx
  ltx <- toCardanoTx' @era network pparams extra tx
  pure $
    evaluateTransactionBalance @era
      pparams
      utxo
      (const True)
      -- TODO this is sort of wrong
      -- if psm starts supporting staking
      -- this would need to be fixed
      (getTxBody @era ltx)

evaluateScriptsInTx ::
  forall era.
  ( Ledger.AlonzoEraTx era
  , HasField "_protocolVersion" (Ledger.PParams era) Ledger.ProtVer
  , HasField "_maxTxExUnits" (Ledger.PParams era) Alonzo.ExUnits
  , Ledger.Script era ~ Alonzo.AlonzoScript era
  , ExtendedUTxO era
  , IsCardanoTx era
  ) =>
  Map TxOutRef TxOut ->
  Ledger.PParams era ->
  Ledger.Network ->
  Tx ->
  P.Extra ->
  Either (Either ToCardanoError (TranslationError (Ledger.Crypto era))) Alonzo.ExUnits
evaluateScriptsInTx utxos pparams network tx extra = do
  ltx <- leftMap Left $ toCardanoTx' @era network pparams extra tx
  utxo <- leftMap Left $ utxoForTransaction @era utxos network tx
  res <-
    leftMap Right $
      evaluateTransactionExecutionUnits @era
        pparams
        ltx
        utxo
        (error "TODO epoch info")
        (error "Time SystemStart")
        (error "TODO cost model")
  pure $ undefined res

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right
