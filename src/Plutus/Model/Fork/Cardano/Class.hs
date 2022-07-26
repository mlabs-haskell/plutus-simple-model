-- | Common interface for Plutus to Cardano conversions for mock Blockchain
module Plutus.Model.Fork.Cardano.Class(
  IsCardanoTx(..),
  toUtxo,
) where

import Prelude

import Data.Map (Map)
import Data.Map qualified as Map
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Era qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Shelley.UTxO qualified as C
import Plutus.V2.Ledger.Api qualified as P
import Plutus.Model.Fork.TxExtra qualified as P
import Plutus.Model.Fork.Cardano.Common (ToCardanoError)
import Plutus.Model.Fork.Ledger.Scripts qualified as C
import Plutus.Model.Fork.Ledger.Tx qualified as Plutus
import Plutus.Model.Fork.Cardano.Common (toTxIn)

class (C.Crypto era ~ StandardCrypto) => IsCardanoTx era where
  toCardanoTx ::
       Map P.ScriptHash (C.Versioned P.Script)
    -> Network
    -> C.PParams era
    -> P.Tx
    -> Either ToCardanoError (C.Tx era)

  toTxOut ::
       Map P.ScriptHash (C.Versioned P.Script)
    -> Network
    -> P.TxOut
    -> Either ToCardanoError (C.TxOut era)

  getTxBody :: C.Tx era -> C.TxBody era


toUtxo ::
     (IsCardanoTx era)
  => Map P.ScriptHash (C.Versioned P.Script)
  -> Network -> [(Plutus.TxIn, P.TxOut)] -> Either ToCardanoError (C.UTxO era)
toUtxo scriptMap network xs = C.UTxO . Map.fromList <$> mapM go xs
  where
    go (tin, tout) = do
      tinC <- toTxIn (Plutus.txInRef tin)
      toutC <- toTxOut scriptMap network tout
      pure (tinC, toutC)
