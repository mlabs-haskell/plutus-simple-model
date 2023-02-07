{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Suites.Plutus.Model.Script.V1.Test.Game (
  tests,
  initGuessGame,
  makeGuessGame,
  badGuessGame,
) where

import Control.Monad (unless)
import Prelude

import Test.Tasty

import PlutusLedgerApi.V2
import PlutusTx.Prelude qualified as Plutus
import Suites.Plutus.Model.Script.V1.Onchain.Game
import Suites.Plutus.Model.Script.V1.Onchain.Game.Script
import Suites.Plutus.Model.Util

import Plutus.Model

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Game scripts"
    [ good "Init script (Guess game)" initGuessGame
    , good "Spend script (Guess game)" makeGuessGame
    , bad "Bad guess" badGuessGame
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

initGuessGame :: Run ()
initGuessGame = do
  users <- setupUsers
  let u1 = head users
      answer = gameSecret
      prize = adaValue 100
  initGame u1 prize answer
  gameUtxos <- utxoAt gameScript
  let [(gameRef, _gameOut)] = gameUtxos
  mDat <- datumAt @GameDatum gameRef
  unless (mDat == Just (GuessHash $ Plutus.sha2_256 answer)) $
    logError "Constraints violated"

badGuessGame :: Run ()
badGuessGame = makeGuessGameBy gameSecret "bad guess"

makeGuessGame :: Run ()
makeGuessGame = makeGuessGameBy gameSecret gameSecret

gameSecret :: BuiltinByteString
gameSecret = "secret"

makeGuessGameBy :: BuiltinByteString -> BuiltinByteString -> Run ()
makeGuessGameBy secret answer = do
  users <- setupUsers
  let [u1, u2, _] = users
  initGame u1 (adaValue 100) secret
  guess u2 answer
  vals <- mapM valueAt users
  let [v1, v2, _] = vals
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $
    logError "Constraint error"

initGame :: PubKeyHash -> Value -> BuiltinByteString -> Run ()
initGame pkh prize answer =
  checkBalance (gives pkh prize gameScript) $ do
    sp <- spend pkh prize
    submitTx pkh $ initGameTx sp prize answer

initGameTx :: UserSpend -> Value -> BuiltinByteString -> Tx
initGameTx usp val answer =
  mconcat
    [ userSpend usp
    , payToScript gameScript (HashDatum $ GuessHash $ Plutus.sha2_256 answer) val
    ]

guess :: PubKeyHash -> BuiltinByteString -> Run ()
guess pkh answer = do
  utxos <- utxoAt gameScript
  let [(gameRef, gameOut)] = utxos
  mDat <- datumAt @GameDatum gameRef
  case mDat of
    Just dat -> checkBalance (gives gameScript (txOutValue gameOut) pkh) $ do
      submitTx pkh $ guessTx pkh gameRef (txOutValue gameOut) dat answer
    Nothing -> logError "No datum"

guessTx :: PubKeyHash -> TxOutRef -> Value -> GameDatum -> BuiltinByteString -> Tx
guessTx pkh gameRef gameVal dat answer =
  mconcat
    [ spendScript gameScript gameRef (Guess answer) dat
    , payToKey pkh gameVal
    ]
