-- | Common interface for Plutus to Cardano conversions for mock Blockchain
module Cardano.Simple.Cardano.Class (
  IsCardanoTx (..),
  toUtxo,
  toCardanoTx',
) where

import Prelude

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.UTxO qualified as C
import Cardano.Simple.Cardano.Common (ToCardanoError, toTxIn)
import Cardano.Simple.Ledger.Scripts qualified as C
import Cardano.Simple.Ledger.Tx qualified as Plutus
import Cardano.Simple.PlutusLedgerApi.V1.Scripts qualified as P
import Cardano.Simple.TxExtra qualified as P
import Data.Map (Map)
import Data.Map qualified as Map
import PlutusLedgerApi.V2 qualified as P

class (C.Crypto era ~ StandardCrypto) => IsCardanoTx era where
  toCardanoTx ::
    Map P.ScriptHash (C.Versioned P.Script) ->
    Network ->
    C.PParams era ->
    P.Extra ->
    Plutus.Tx ->
    Either ToCardanoError (C.Tx era)

  toTxOut ::
    Map P.ScriptHash (C.Versioned P.Script) ->
    Network ->
    P.TxOut ->
    Either ToCardanoError (C.TxOut era)

  getTxBody :: C.Tx era -> C.TxBody era

-- TODO better name
toCardanoTx' ::
  IsCardanoTx era =>
  Network ->
  C.PParams era ->
  P.Extra ->
  Plutus.Tx ->
  Either ToCardanoError (C.Tx era)
toCardanoTx' n p e tx =
  toCardanoTx (Plutus.txScripts tx) n p e tx

toUtxo ::
  (IsCardanoTx era) =>
  Map P.ScriptHash (C.Versioned P.Script) ->
  Network ->
  [(Plutus.TxIn, P.TxOut)] ->
  Either ToCardanoError (C.UTxO era)
toUtxo scriptMap network xs = C.UTxO . Map.fromList <$> mapM go xs
  where
    go (tin, tout) = do
      tinC <- toTxIn (Plutus.txInRef tin)
      toutC <- toTxOut scriptMap network tout
      pure (tinC, toutC)
