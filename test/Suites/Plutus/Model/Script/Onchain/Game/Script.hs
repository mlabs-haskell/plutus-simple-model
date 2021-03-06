module Suites.Plutus.Model.Script.Onchain.Game.Script (
  Game,
  gameScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.Onchain.Game
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Game = TypedValidator GameDatum GameAct

-- | The GeroGov validator script instance
gameScript :: Game
gameScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||])
