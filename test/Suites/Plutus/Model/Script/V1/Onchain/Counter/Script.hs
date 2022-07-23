module Suites.Plutus.Model.Script.V1.Onchain.Counter.Script (
  Counter,
  counterScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.V1.Onchain.Counter
import Plutus.Test.Model.V1 (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Counter = TypedValidator CounterDatum CounterAct

-- | The GeroGov validator script instance
counterScript :: Counter
counterScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator counterContract ||])
