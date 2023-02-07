{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Guess hash Game with inlined datum
module Suites.Plutus.Model.Script.V2.Test.Game (
  tests,
) where

import Control.Monad (unless)

import Data.Either
import Data.Functor (void)
import Prelude

import Test.Tasty

import PlutusLedgerApi.V2
import PlutusTx.Prelude qualified as Plutus
import Suites.Plutus.Model.Script.V2.Onchain.Game
import Suites.Plutus.Model.Script.V2.Onchain.Game.Script
import Suites.Plutus.Model.Util

import Plutus.Model

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Game scripts (Inline datum)"
    [ good "Spend script (Guess game)" makeGuessGame
    , bad "Bad guess" badGuessGame
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

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
  postedTx <- guess u2 answer
  vals <- mapM valueAt users
  let [v1, v2, _] = vals
  unless (postedTx && v1 == adaValue 900 && v2 == adaValue 1100) $
    logError "Constraint error"

initGame :: PubKeyHash -> Value -> BuiltinByteString -> Run ()
initGame pkh prize answer =
  checkBalance (gives pkh prize gameScript) $ do
    sp <- spend pkh prize
    tx <- signTx pkh $ initGameTx sp prize answer
    void $ sendTx tx

initGameTx :: UserSpend -> Value -> BuiltinByteString -> Tx
initGameTx usp val answer =
  mconcat
    [ userSpend usp
    , payToScript gameScript (InlineDatum $ GuessHash $ Plutus.sha2_256 answer) val
    ]

guess :: PubKeyHash -> BuiltinByteString -> Run Bool
guess pkh answer = do
  utxos <- utxoAt gameScript
  let [(gameRef, gameOut)] = utxos
      mDat = getInlineDatum gameOut
  case mDat of
    Just dat -> checkBalance (gives gameScript (txOutValue gameOut) pkh) $ do
      tx <- signTx pkh $ guessTx pkh gameRef (txOutValue gameOut) dat answer
      isRight <$> sendTx tx
    Nothing -> pure False

guessTx :: PubKeyHash -> TxOutRef -> Value -> GameDatum -> BuiltinByteString -> Tx
guessTx pkh gameRef gameVal dat answer =
  mconcat
    [ spendScript gameScript gameRef (Guess answer) dat
    , payToKey pkh gameVal
    ]
