module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Script (
  Bet,
  betScript,
) where

import Prelude (($))
import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Oracle
import Plutus.Test.Model.V2 (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Bet = TypedValidator BetDatum BetAct

-- | The GeroGov validator script instance
betScript :: BetParams -> Bet
betScript betParams =
  mkTypedValidator $
    $$(PlutusTx.compile [|| \param -> toBuiltinValidator (betContract param) ||])
      `PlutusTx.applyCode` PlutusTx.liftCode betParams
