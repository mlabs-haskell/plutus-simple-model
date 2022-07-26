-- | Oracle example with script that uses hashed datum for Oracle reference input.
module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Hashed (
  Bet,
  betScript,
) where

import Prelude (($))
import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Oracle
import Plutus.Model.V2 (toBuiltinValidator, TypedValidator, mkTypedValidator)
import Suites.Plutus.Model.Script.V2.Onchain.Util

type Bet = TypedValidator BetDatum BetAct

-- | The GeroGov validator script instance
betScript :: BetParams -> Bet
betScript betParams =
  mkTypedValidator $
    $$(PlutusTx.compile [|| \param -> toBuiltinValidator (betContract datumOf param) ||])
      `PlutusTx.applyCode` PlutusTx.liftCode betParams
