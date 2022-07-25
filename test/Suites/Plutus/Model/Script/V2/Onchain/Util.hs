module Suites.Plutus.Model.Script.V2.Onchain.Util(
  datumOf,
  inlinedDatum,
  lovelaceValueOf,
  getThrough,
  forwardTo,
) where

import PlutusTx.Prelude
import PlutusTx.AssocMap qualified as Map
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts

{-# inlinable getThrough #-}
getThrough :: ScriptContext -> (TxOut, TxOut)
getThrough ctx = (tin, tout)
  where
    [tout] = getContinuingOutputs ctx
    Just tinInfo = findOwnInput ctx
    tin = txInInfoResolved tinInfo

{-# inlinable lovelaceValueOf #-}
lovelaceValueOf :: Value -> Integer
lovelaceValueOf v = valueOf v adaSymbol adaToken

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

{-# inlinable forwardTo #-}
-- | check that script is spent with given redeemer
forwardTo :: ToData redeemer => ValidatorHash -> redeemer -> TxInfo -> Bool
forwardTo vh redeemer info =
  (Map.lookup (Spending ref) (txInfoRedeemers info) == Just (Redeemer $ toBuiltinData redeemer))
  where
    Just inInfo = find ((== addr) . txOutAddress . txInInfoResolved) $ txInfoInputs info
    !addr = scriptHashAddress vh
    !ref = txInInfoOutRef inInfo

