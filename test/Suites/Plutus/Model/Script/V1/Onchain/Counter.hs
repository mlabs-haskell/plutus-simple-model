-- | Simple counter that increments internal counter on every usage in TX.
module Suites.Plutus.Model.Script.V1.Onchain.Counter(
  CounterDatum(..),
  CounterAct(..),
  counterContract,
) where

import Prelude

import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Contexts

import Plutus.Model.V1 (datumOf)

----------------------------------------------------------------------------
-- types

newtype CounterDatum = CounterDatum { getCounterDatum :: Plutus.Integer }
  deriving newtype (ToData, FromData, UnsafeFromData, Plutus.Eq, Eq, Show)

data CounterAct = Bump

----------------------------------------------------------------------------
-- contract

{-# inlinable counterContract #-}
counterContract :: CounterDatum -> CounterAct -> ScriptContext -> Bool
counterContract (CounterDatum n) Bump ctx =
  case datumOf info counterOut of
    Just (CounterDatum m) -> Plutus.traceIfFalse "Counter is incremented" (n Plutus.+1 Plutus.== m)
    Nothing               -> Plutus.traceError "No datum"
  where
    !info = scriptContextTxInfo ctx
    [!counterOut] = getContinuingOutputs ctx

----------------------------------------------------------------------------
-- instances

PlutusTx.makeIsDataIndexed ''CounterAct [('Bump, 0)]
