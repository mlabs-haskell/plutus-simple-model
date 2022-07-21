module Suites.Plutus.Model.Script.Onchain.Counter.Script (
  Counter,
  counterScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.Onchain.Counter
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidatorV1)

type Counter = TypedValidator CounterDatum CounterAct

-- | The GeroGov validator script instance
counterScript :: Counter
counterScript = mkTypedValidatorV1 $$(PlutusTx.compile [|| toBuiltinValidator counterContract ||])
