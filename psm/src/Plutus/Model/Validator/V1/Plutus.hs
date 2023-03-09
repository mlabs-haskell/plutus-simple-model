{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Utility functions for Plutus V1 scripts
module Plutus.Model.Validator.V1.Plutus (
  getThrough,
  datumOf,
) where

import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Contexts
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
