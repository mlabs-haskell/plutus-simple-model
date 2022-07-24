module Suites.Plutus.Model.Script.V2.Onchain.Util(
  datumOf,
  inlinedDatum,
) where

import PlutusTx.Prelude
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts

{-# inlinable datumOf #-}
datumOf :: FromData a => TxInfo -> TxOut -> Maybe a
datumOf info tout = do
  dh <- txOutDatumHash tout
  dat <- getDatum <$> findDatum dh info
  fromBuiltinData dat

{-# inlinable txOutDatumHash #-}
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash tout =
  case txOutDatum tout of
    OutputDatumHash dh -> Just dh
    _                  -> Nothing

{-# inlinable inlinedDatum #-}
inlinedDatum :: FromData a => TxOut -> Maybe a
inlinedDatum tout = do
  d <- case txOutDatum tout of
    OutputDatum (Datum dat) -> Just dat
    _                       -> Nothing
  fromBuiltinData d



