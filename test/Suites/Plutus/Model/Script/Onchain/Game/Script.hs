module Suites.Plutus.Model.Script.Onchain.Game.Script (
  gameScript
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.Onchain.Game
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidator)

-- | The GeroGov validator script instance
gameScript :: TypedValidator GameDatum GameAct
gameScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||])
