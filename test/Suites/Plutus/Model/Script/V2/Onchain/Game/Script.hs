module Suites.Plutus.Model.Script.V2.Onchain.Game.Script (
  Game,
  gameScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Game
import Plutus.Test.Model.V2 (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Game = TypedValidator GameDatum GameAct

-- | The GeroGov validator script instance
gameScript :: Game
gameScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||])
