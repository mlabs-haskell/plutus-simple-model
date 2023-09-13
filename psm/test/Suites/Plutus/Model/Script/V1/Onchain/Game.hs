{- | Onchain code for guess a hash game.

 User can submit value to script that is locked by the hash of some secret.
 If other user can guess the hash then user can grab the value.
-}
module Suites.Plutus.Model.Script.V1.Onchain.Game (
  GameDatum (..),
  GameAct (..),
  gameContract,
) where

import PlutusLedgerApi.V1
import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus
import qualified Prelude

newtype GameDatum = GuessHash Plutus.BuiltinByteString deriving Prelude.Eq

newtype GameAct = Guess Plutus.BuiltinByteString

{-# INLINEABLE gameContract #-}
gameContract :: GameDatum -> GameAct -> ScriptContext -> Plutus.Bool
gameContract (GuessHash h) (Guess answer) _ =
  Plutus.sha2_256 answer Plutus.== h

PlutusTx.unstableMakeIsData ''GameDatum
PlutusTx.unstableMakeIsData ''GameAct
