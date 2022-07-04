module Suites.Plutus.Model.Script.Onchain.Game.Script (
  gameScript
) where

import Plutus.V1.Ledger.Api
import PlutusTx qualified
import Suites.Plutus.Model.Script.Onchain.Game
import Plutus.Test.Model (toBuiltinValidator, TypedValidator(..))

-- | The GeroGov validator script instance
gameScript :: TypedValidator GameDatum GameAct
gameScript = TypedValidator (mkValidatorScript $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||]))
