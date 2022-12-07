-- | Compiled script for Hash Game example
module Suites.Plutus.Model.Script.V2.Onchain.Game.Script (
  Game,
  gameScript,
) where

import Plutus.Model.V2 (TypedValidator) -- , mkTypedValidator, toBuiltinValidator)
-- import PlutusTx qualified

import PlutusTx.Builtins (error)
import Suites.Plutus.Model.Script.V2.Onchain.Game

type Game = TypedValidator GameDatum GameAct

-- | The GeroGov validator script instance
gameScript :: Game
gameScript = error ()

-- gameScript = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator gameContract||])
