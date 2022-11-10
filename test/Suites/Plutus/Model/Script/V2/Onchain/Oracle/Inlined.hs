-- | Oracle example with script that uses inlined datum for Oracle reference input.
module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Inlined (
  Bet,
  betScript,
) where

import Plutus.Model.V2 (TypedValidator) --, inlinedDatum, mkTypedValidator, toBuiltinValidator)
-- import PlutusTx qualified
-- import PlutusTx.Prelude (const)
import Suites.Plutus.Model.Script.V2.Onchain.Oracle
-- import Prelude (($))
import PlutusTx.Builtins (error)

type Bet = TypedValidator BetDatum BetAct

-- | The TypedValidator for Bet on Oracle answer contract
betScript :: BetParams -> Bet
betScript _betParams = error ()
  -- mkTypedValidator $
  --   $$(PlutusTx.compile [||\param -> toBuiltinValidator (betContract (const inlinedDatum) param)||])
  --     `PlutusTx.applyCode` PlutusTx.liftCode betParams
