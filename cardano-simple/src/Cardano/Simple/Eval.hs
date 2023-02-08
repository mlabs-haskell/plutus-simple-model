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
import Cardano.Ledger.Language qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger

import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo

import Cardano.Simple.Cardano.Class (
  IsCardanoTx,
  toUtxo,
 )
import Cardano.Simple.Cardano.Common (ToCardanoError)
import Cardano.Simple.Ledger.Scripts qualified as C
import Cardano.Simple.Ledger.Tx (
  Tx (txCollateral, txInputs, txReferenceInputs),
  TxIn (TxIn),
 )
import PlutusLedgerApi.V2 (TxOut, TxOutRef)

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
  Map P.ScriptHash (C.Versioned P.Script) ->
  Map TxOutRef TxOut ->
  Ledger.Network ->
  Tx ->
  Either ToCardanoError (Ledger.UTxO era)
utxoForTransaction scripts utxos network tx =
  toUtxo @era scripts network inOutList
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
  Ledger.PParams era ->
  Map P.ScriptHash (C.Versioned P.Script) ->
  Ledger.Network ->
  Tx ->
  Ledger.Value era
txBalance = error "TODO"
