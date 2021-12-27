{- | Onchain code for guess a hash game.

 User can submit value to script that is locked by the hash of some secret.
 If other user can guess the hash then user can grab the value.
-}
module Suites.Plutus.Model.Script.Onchain.Game (
  Game,
  GameDatum (..),
  GameAct (..),
  gameContract,
  gameScript,
  gameValidator,
  gameAddress,
) where

import Prelude

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

data Game

data GameDatum = GuessHash Plutus.BuiltinByteString
  deriving (Eq)

data GameAct = Guess Plutus.BuiltinByteString

instance Scripts.ValidatorTypes Game where
  type DatumType Game = GameDatum
  type RedeemerType Game = GameAct

gameContract :: GameDatum -> GameAct -> Ledger.ScriptContext -> Bool
gameContract (GuessHash h) (Guess answer) _ =
  Plutus.sha2_256 answer Plutus.== h

-- | The GeroGov validator script instance
gameScript :: Scripts.TypedValidator Game
gameScript =
  Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [||gameContract||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameAct

-- | The validator of the GeroGov script
gameValidator :: Scripts.Validator
gameValidator = Scripts.validatorScript gameScript

-- | The script address of the GeroGov script
gameAddress :: Ledger.Address
gameAddress = Ledger.scriptAddress gameValidator

PlutusTx.unstableMakeIsData ''GameDatum
PlutusTx.unstableMakeIsData ''GameAct
