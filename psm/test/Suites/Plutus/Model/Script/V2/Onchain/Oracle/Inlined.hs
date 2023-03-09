-- | Oracle example with script that uses inlined datum for Oracle reference input.
module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Inlined (
  Bet,
  betScript,
) where

import Plutus.Model.V2 (TypedValidator, inlinedDatum, mkTypedValidator, toBuiltinValidator)
import PlutusTx qualified
import PlutusTx.Prelude (const)

import Suites.Plutus.Model.Script.V2.Onchain.Oracle

import Prelude (($))

type Bet = TypedValidator BetDatum BetAct

{- HLINT ignore betScript "Avoid lambda" -}

-- | The TypedValidator for Bet on Oracle answer contract
betScript :: BetParams -> Bet
betScript betParams =
  mkTypedValidator $
    $$(PlutusTx.compile [||\param -> toBuiltinValidator (betContract (const inlinedDatum) param)||])
      `PlutusTx.applyCode` PlutusTx.liftCode betParams
