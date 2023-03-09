-- | Compiled script for Counter example
module Suites.Plutus.Model.Script.V1.Onchain.Counter.Script (
  Counter,
  counterScript,
) where

import Plutus.Model.V1 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import PlutusTx qualified

import Suites.Plutus.Model.Script.V1.Onchain.Counter

type Counter = TypedValidator CounterDatum CounterAct

-- | The TypedValidator for counter contract
counterScript :: Counter
counterScript = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator counterContract||])
