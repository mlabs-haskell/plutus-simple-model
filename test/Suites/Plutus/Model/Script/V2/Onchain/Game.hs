{- | Onchain code for guess a hash game.

 User can submit value to script that is locked by the hash of some secret.
 If other user can guess the hash then user can grab the value.
-}
module Suites.Plutus.Model.Script.V2.Onchain.Game (
  GameDatum (..),
  GameAct (..),
  gameContract,
) where

import PlutusLedgerApi.V2
import PlutusTx qualified
import PlutusTx.Prelude

newtype GameDatum = GuessHash BuiltinByteString

newtype GameAct = Guess BuiltinByteString

{-# INLINEABLE gameContract #-}
gameContract :: GameDatum -> GameAct -> ScriptContext -> Bool
gameContract (GuessHash h) (Guess answer) _ =
  traceIfFalse "Wrong guess" $ sha2_256 answer == h

PlutusTx.unstableMakeIsData ''GameDatum
PlutusTx.unstableMakeIsData ''GameAct
