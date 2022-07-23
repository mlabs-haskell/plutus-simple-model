module Suites.Plutus.Model.Script.V1.Onchain.Util(
  datumOf
) where

import PlutusTx.Prelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts

{-# inlinable datumOf #-}
datumOf :: FromData a => TxInfo -> TxOut -> Maybe a
datumOf info tout = do
  dh <- txOutDatumHash tout
  dat <- getDatum <$> findDatum dh info
  fromBuiltinData dat

