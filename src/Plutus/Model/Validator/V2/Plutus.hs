-- | Utility functions for Plutus V2 scripts
module Plutus.Model.Validator.V2.Plutus (
  datumOf,
  inlinedDatum,
  getThrough,
  forwardTo,
) where

import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude

{-# INLINEABLE getThrough #-}
getThrough :: ScriptContext -> (TxOut, TxOut)
getThrough ctx = (tin, tout)
  where
    tout = head (getContinuingOutputs ctx)
    tinInfo = fromMaybe (error ()) (findOwnInput ctx)
    tin = txInInfoResolved tinInfo

{-# INLINEABLE datumOf #-}
datumOf :: FromData a => TxInfo -> TxOut -> Maybe a
datumOf info tout = do
  dh <- txOutDatumHash tout
  dat <- getDatum <$> findDatum dh info
  fromBuiltinData dat

{-# INLINEABLE txOutDatumHash #-}
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash tout =
  case txOutDatum tout of
    OutputDatumHash dh -> Just dh
    _ -> Nothing

{-# INLINEABLE inlinedDatum #-}
inlinedDatum :: FromData a => TxOut -> Maybe a
inlinedDatum tout = do
  d <- case txOutDatum tout of
    OutputDatum (Datum dat) -> Just dat
    _ -> Nothing
  fromBuiltinData d

{-# INLINEABLE forwardTo #-}

-- | check that script is spent with given redeemer
forwardTo :: ToData redeemer => ScriptHash -> redeemer -> TxInfo -> Bool
forwardTo vh redeemer info =
  Map.lookup (Spending ref) (txInfoRedeemers info) == Just (Redeemer $ toBuiltinData redeemer)
  where
    inInfo = fromMaybe (error ()) . find ((== addr) . txOutAddress . txInInfoResolved) $ txInfoInputs info
    !addr = scriptHashAddress vh
    !ref = txInInfoOutRef inInfo
