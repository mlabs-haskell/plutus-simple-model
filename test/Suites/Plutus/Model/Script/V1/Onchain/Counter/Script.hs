module Suites.Plutus.Model.Script.V1.Onchain.Counter.Script (
  Counter,
  counterScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.V1.Onchain.Counter
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidatorV1)

type Counter = TypedValidator CounterDatum CounterAct

-- | The GeroGov validator script instance
counterScript :: Counter
counterScript = mkTypedValidatorV1 $$(PlutusTx.compile [|| toBuiltinValidator counterContract ||])
