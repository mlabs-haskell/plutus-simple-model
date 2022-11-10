-- | Compiled script for Safe example
module Suites.Plutus.Model.Script.V1.Onchain.Safe.Script (
  Safe,
  safeScript,
) where

import Plutus.Model.V1 (TypedValidator) --, mkTypedValidator, toBuiltinValidator)
-- import PlutusTx qualified
import Suites.Plutus.Model.Script.V1.Onchain.Safe
import PlutusTx.Builtins (error)

type Safe = TypedValidator SafeDatum SafeAct

-- | The TypedValidator for Safe contract
safeScript :: SafeParams -> Safe
safeScript _params = error ()
  -- mkTypedValidator
  --   ( $$(PlutusTx.compile [||\ps -> toBuiltinValidator (safeContract ps)||])
  --       `PlutusTx.applyCode` PlutusTx.liftCode params
  --   )
