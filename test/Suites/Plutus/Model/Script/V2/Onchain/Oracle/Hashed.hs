-- | Oracle example with script that uses hashed datum for Oracle reference input.
module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Hashed (
  Bet,
  betScript,
) where

import Plutus.Model.V2 (TypedValidator) --, datumOf, mkTypedValidator, toBuiltinValidator)
-- import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Oracle
-- import Prelude (($))
import PlutusTx.Builtins (error)

type Bet = TypedValidator BetDatum BetAct

-- | The GeroGov validator script instance
betScript :: BetParams -> Bet
betScript _betParams = error ()
  -- mkTypedValidator $
  --   $$(PlutusTx.compile [||\param -> toBuiltinValidator (betContract datumOf param)||])
  --     `PlutusTx.applyCode` PlutusTx.liftCode betParams
