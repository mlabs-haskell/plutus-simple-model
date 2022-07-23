module Suites.Plutus.Model.Script.V1.Onchain.Safe.Script (
  Safe,
  safeScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.V1.Onchain.Safe
import Plutus.Test.Model.V1 (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Safe = TypedValidator SafeDatum SafeAct

-- | The GeroGov validator script instance
safeScript :: SafeParams -> Safe
safeScript params = mkTypedValidator (
  $$(PlutusTx.compile [|| \ps -> toBuiltinValidator (safeContract ps) ||])
  `PlutusTx.applyCode` PlutusTx.liftCode params)
