module Plutus.Test.Model.Fork.Cardano.Alonzo(
  Era,
  toAlonzoTx,
  fromTxId,
) where

import Prelude

import Cardano.Ledger.TxIn qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Alonzo (AlonzoEra, PParams)
import Cardano.Ledger.Alonzo.Tx qualified as C
import Plutus.Test.Model.Fork.TxExtra qualified as P
import Plutus.V1.Ledger.Tx qualified as P
import PlutusTx.Builtins.Internal qualified as P

import Cardano.Ledger.SafeHash
import Cardano.Crypto.Hash.Class
import Data.ByteString.Short (fromShort)

type Era = AlonzoEra StandardCrypto

toAlonzoTx :: PParams Era -> P.Tx -> Either String (C.ValidatedTx Era)
toAlonzoTx _params _tx = undefined

fromTxId :: C.TxId StandardCrypto -> P.TxId
fromTxId (C.TxId safeHash) =
  case extractHash safeHash of
    UnsafeHash shortBs -> P.TxId $ P.BuiltinByteString $ fromShort shortBs



