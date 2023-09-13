-- | Compiled script for Safe example
module Suites.Plutus.Model.Script.V1.Onchain.Safe.Script (
  Safe,
  safeScript,
) where

import Plutus.Model.V1 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import PlutusTx qualified

import Suites.Plutus.Model.Script.V1.Onchain.Safe
import PlutusCore.Version (plcVersion100)

type Safe = TypedValidator SafeDatum SafeAct

{- HLINT ignore safeScript "Avoid lambda" -}

-- | The TypedValidator for Safe contract
safeScript :: SafeParams -> Safe
safeScript params =
  mkTypedValidator
    ( $$(PlutusTx.compile [||\ps -> toBuiltinValidator (safeContract ps)||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
    )
