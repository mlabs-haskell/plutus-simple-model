module Suites.Plutus.Model.Script.Onchain.Util (
  datumOf,
) where

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import PlutusTx.Prelude

{-# INLINEABLE datumOf #-}
datumOf :: FromData a => TxInfo -> TxOut -> Maybe a
datumOf info tout = do
  dh <- txOutDatumHash tout
  dat <- getDatum <$> findDatum dh info
  fromBuiltinData dat
