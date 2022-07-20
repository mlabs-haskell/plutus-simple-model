-- | Common interface for Plutus to Cardano conversions for mock Blockchain
module Plutus.Test.Model.Fork.Cardano.Class(
  IsCardanoTx(..)
) where

import Prelude

import Data.Map (Map)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core qualified as C
import Plutus.V2.Ledger.Api qualified as P
import Plutus.Test.Model.Fork.TxExtra qualified as P
import Plutus.Test.Model.Fork.Cardano.Common (ToCardanoError)
import Plutus.Test.Model.Fork.Ledger.Scripts qualified as C

class IsCardanoTx era where
  toCardanoTx ::
       Map P.ScriptHash (C.Versioned P.Script)
    -> Network
    -> C.PParams era
    -> P.Tx
    -> Either ToCardanoError (C.Tx era)

  toCardanoTxOut ::
       Map P.ScriptHash (C.Versioned P.Script)
    -> Network
    -> P.TxOut
    -> Either ToCardanoError (C.TxOut era)

