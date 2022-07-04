module Suites.Plutus.Model.Script.Onchain.Safe.Script (
  Safe,
  safeScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.Onchain.Safe
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Safe = TypedValidator SafeDatum SafeAct

-- | The GeroGov validator script instance
safeScript :: SafeParams -> Safe
safeScript params = mkTypedValidator (
  $$(PlutusTx.compile [|| \ps -> toBuiltinValidator (safeContract ps) ||])
  `PlutusTx.applyCode` PlutusTx.liftCode params)
