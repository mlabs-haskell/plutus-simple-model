module Suites.Plutus.Model.Script.Onchain.Counter.Script (
  Counter,
  counterScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.Onchain.Counter
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Counter = TypedValidator CounterDatum CounterAct

-- | The GeroGov validator script instance
counterScript :: Counter
counterScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator counterContract ||])
