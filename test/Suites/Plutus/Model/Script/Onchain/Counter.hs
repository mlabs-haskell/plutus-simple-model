-- | Simple counter that increments internal counter on every usage in TX.
module Suites.Plutus.Model.Script.Onchain.Counter(
  Counter,
  CounterDatum(..),
  CounterAct(..),
  counterContract,
  counterScript,
  counterValidator,
  counterAddress,
) where

import Prelude

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts

import Suites.Plutus.Model.Script.Onchain.Util (datumOf)

----------------------------------------------------------------------------
-- types

data Counter

instance Scripts.ValidatorTypes Counter where
  type DatumType Counter = CounterDatum
  type RedeemerType Counter = CounterAct

newtype CounterDatum = CounterDatum { getCounterDatum :: Integer }
  deriving (ToData, FromData, UnsafeFromData, Plutus.Eq, Eq, Show)

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
-- compiled code

-- | The GeroGov validator script instance
counterScript :: Scripts.TypedValidator Counter
counterScript =
  Scripts.mkTypedValidator @Counter
    $$(PlutusTx.compile [||counterContract||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @CounterDatum @CounterAct

-- | The validator of the GeroGov script
counterValidator :: Scripts.Validator
counterValidator = Scripts.validatorScript counterScript

-- | The script address of the GeroGov script
counterAddress :: Ledger.Address
counterAddress = Ledger.scriptAddress counterValidator

----------------------------------------------------------------------------
-- instances

PlutusTx.makeIsDataIndexed ''CounterAct [('Bump, 0)]
