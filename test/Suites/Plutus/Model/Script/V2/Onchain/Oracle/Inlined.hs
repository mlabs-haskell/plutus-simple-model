-- | Oracle example with script that uses inlined datum for Oracle reference input.
module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Inlined (
  Bet,
  betScript,
) where

import Prelude (($))
import PlutusTx.Prelude (const)
import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Oracle
import Plutus.Model.V2 (toBuiltinValidator, TypedValidator, mkTypedValidator)
import Suites.Plutus.Model.Script.V2.Onchain.Util

type Bet = TypedValidator BetDatum BetAct

-- | The TypedValidator for Bet on Oracle answer contract
betScript :: BetParams -> Bet
betScript betParams =
  mkTypedValidator $
    $$(PlutusTx.compile [|| \param -> toBuiltinValidator (betContract (const inlinedDatum) param) ||])
      `PlutusTx.applyCode` PlutusTx.liftCode betParams
