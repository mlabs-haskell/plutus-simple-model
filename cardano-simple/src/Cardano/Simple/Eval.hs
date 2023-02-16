module Cardano.Simple.Eval (
  evalScript,
  utxoForTransaction,
  txBalance,
  evaluateScriptsInTx,
  toAlonzoCostModels,
) where

import Prelude

import Data.Array qualified as Array
import Data.Either (lefts, rights)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Records (HasField (getField))

import Cardano.Ledger.Alonzo.Data qualified as Ledger
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Language qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger

import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Slotting.EpochInfo.Impl (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), slotLengthFromMillisec)

import Cardano.Ledger.Alonzo.Tools (evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, TranslationError)
import Cardano.Ledger.Shelley.API (CLI, evaluateTransactionBalance)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody)

import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo

import Cardano.Simple.Cardano.Class (
  IsCardanoTx,
  getTxBody,
  toCardanoTx,
  toUtxo,
 )
import Cardano.Simple.Cardano.Common (ToCardanoError)
import Cardano.Simple.Ledger.TimeSlot (SlotConfig, scSlotLength, scSlotZeroTime)
import Cardano.Simple.Ledger.Tx (
  Tx (txCollateral, txInputs, txReferenceInputs, txScripts),
  TxIn (TxIn),
 )
import Cardano.Simple.TxExtra (Extra)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1.Time (getPOSIXTime)
import PlutusLedgerApi.V2 (TxOut, TxOutRef)

import Debug.Trace

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
      (Alonzo.transProtocolVersion pparams._protocolVersion)
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
  case inOutList of
    Nothing -> Left "lookup failure"
    Just list -> toUtxo @era (txScripts tx) network list
  where
    inOutList :: Maybe [(TxIn, TxOut)]
    inOutList =
      sequence
        [ (txin,) <$> out
        | txin@(TxIn outRef _) <-
            Set.toList $
              txInputs tx
                <> txCollateral tx
                <> txReferenceInputs tx
        , let out = Map.lookup outRef utxos
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
  Extra ->
  Either ToCardanoError (Ledger.Value era)
txBalance utxos pparams network tx extra = do
  utxo <- utxoForTransaction @era utxos network tx
  ltx <- toCardanoTx @era network pparams extra tx
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
  ( HasField "_protocolVersion" (Ledger.PParams era) Ledger.ProtVer
  , HasField "_maxTxExUnits" (Ledger.PParams era) Alonzo.ExUnits
  , HasField "_costmdls" (Ledger.PParams era) Alonzo.CostModels
  , Ledger.AlonzoEraTx era
  , Ledger.Script era ~ Alonzo.AlonzoScript era
  , ExtendedUTxO era
  , IsCardanoTx era
  ) =>
  Map TxOutRef TxOut ->
  Ledger.PParams era ->
  Ledger.Network ->
  Tx ->
  Extra ->
  SlotConfig ->
  Either
    (Either ToCardanoError (TranslationError (Ledger.Crypto era)))
    Alonzo.ExUnits
evaluateScriptsInTx utxos pparams network tx extra slotCfg = do
  traceM "=====evaluateScriptsInTx========"
  ltx <- leftMap Left $ toCardanoTx @era network pparams extra tx
  traceM $ "++++++ ltx ++++++" <> show ltx
  utxo <- leftMap Left $ utxoForTransaction @era utxos network tx
  traceM $ "++++++ utxo ++++++" <> show utxo
  res <-
    leftMap Right $
      evaluateTransactionExecutionUnits @era
        pparams
        ltx
        utxo
        ( fixedEpochInfo
            (EpochSize 1)
            (slotLengthFromMillisec $ scSlotLength slotCfg)
        )
        ( SystemStart $
            posixSecondsToUTCTime $
              fromInteger $
                (`div` 1000) $
                  getPOSIXTime $
                    scSlotZeroTime slotCfg
        )
        (toAlonzoCostModels $ getField @"_costmdls" pparams)
  traceM "========== evaluateScriptsInTx"
  let res' = (\(k, v) -> fmap (k,) v) <$> Map.toList res
      errs = lefts res'
      cost = foldMap snd . rights $ res'
   in if null errs
        then pure cost
        else Left . Left $ show errs

toAlonzoCostModels ::
  Alonzo.CostModels ->
  Array.Array Alonzo.Language Alonzo.CostModel
toAlonzoCostModels (Alonzo.CostModels costmodels) =
  Array.array (minBound, maxBound) $ Map.toList costmodels

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f = either (Left . f) Right
