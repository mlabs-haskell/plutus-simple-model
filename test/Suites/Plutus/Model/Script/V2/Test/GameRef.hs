{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | Tets for reference scripts. We deploy script for hash game
 to UTxO and use it as reference input
-}
module Suites.Plutus.Model.Script.V2.Test.GameRef (
  tests,
) where

import Test.Tasty
import Prelude

import PlutusLedgerApi.V2
import PlutusTx.Prelude qualified as Plutus
import Suites.Plutus.Model.Script.V2.Onchain.Game
import Suites.Plutus.Model.Script.V2.Onchain.Game.Script
import Suites.Plutus.Model.Util

import Plutus.Model

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Game scripts (Reference script)"
    [ good "Spend script (Guess game)" makeGuessGame
    , bad "Bad guess" badGuessGame
    , bad "Good guess but no script loaded" makeGuessGameNoScript
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

data RefScriptMode
  = LoadScript
  | NoScript

badGuessGame :: Run ()
badGuessGame = makeGuessGameBy LoadScript gameSecret "bad guess"

makeGuessGame :: Run ()
makeGuessGame = makeGuessGameBy LoadScript gameSecret gameSecret

makeGuessGameNoScript :: Run ()
makeGuessGameNoScript = makeGuessGameBy NoScript gameSecret gameSecret

gameSecret :: BuiltinByteString
gameSecret = "secret"

makeGuessGameBy :: RefScriptMode -> BuiltinByteString -> BuiltinByteString -> Run ()
makeGuessGameBy refScriptMode secret answer = do
  users <- setupUsers
  let [u1, u2, _] = users
  initGame refScriptMode u1 (adaValue 100) secret
  guess u2 answer

initGame :: RefScriptMode -> PubKeyHash -> Value -> BuiltinByteString -> Run ()
initGame loadScript pkh prize answer = do
  case loadScript of
    LoadScript -> do
      sp1 <- spend pkh riderAda
      submitTx pkh $ loadGameScriptTx sp1
    NoScript -> pure ()
  sp2 <- spend pkh prize
  submitTx pkh $ loadPrizeTx sp2
  where
    loadGameScriptTx usp =
      mconcat
        [ userSpend usp
        , loadRefScript gameScript riderAda
        ]

    loadPrizeTx usp =
      mconcat
        [ userSpend usp
        , payToRef gameScript (InlineDatum gameDatum) prize
        ]

    gameDatum = GuessHash $ Plutus.sha2_256 answer

guess :: PubKeyHash -> BuiltinByteString -> Run ()
guess pkh answer =
  withFirstRefScript gameScript $ \(refScript, _) ->
    withFirstUtxo gameScript $ \(gameRef, gameOut) -> do
      case getInlineDatum gameOut of
        Just dat -> do
          submitTx pkh $ guessTx refScript pkh gameRef (txOutValue gameOut) dat answer
        Nothing -> logError "No datum"

guessTx :: TxOutRef -> PubKeyHash -> TxOutRef -> Value -> GameDatum -> BuiltinByteString -> Tx
guessTx refScript pkh gameRef gameVal dat answer =
  mconcat
    [ spendScriptRef refScript gameScript gameRef (Guess answer) dat
    , payToKey pkh gameVal
    ]
