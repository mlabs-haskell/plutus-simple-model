-- | Compiled script for Counter example
module Suites.Plutus.Model.Script.V1.Onchain.Counter.Script (
  Counter,
  counterScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.V1.Onchain.Counter
import Plutus.Model.V1 (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Counter = TypedValidator CounterDatum CounterAct

-- | The TypedValidator for counter contract
counterScript :: Counter
counterScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator counterContract ||])
