module Suites.Plutus.Model.Script.V1.Onchain.Game.Script (
  Game,
  gameScript,
) where

import PlutusTx qualified
import Suites.Plutus.Model.Script.V1.Onchain.Game
import Plutus.Test.Model (toBuiltinValidator, TypedValidator, mkTypedValidatorV1)

type Game = TypedValidator GameDatum GameAct

-- | The GeroGov validator script instance
gameScript :: Game
gameScript = mkTypedValidatorV1 $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||])
