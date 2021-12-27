module Suites.Plutus.Model.Script.Test.Game (
  tests,
  initGuessGame,
  makeGuessGame,
) where

import Data.Either
import Data.Functor (void)
import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Tx
import PlutusTx.Prelude qualified as Plutus
import Suites.Plutus.Model.Script.Onchain.Game
import Suites.Plutus.Model.Util

import Plutus.Test.Model

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Game scripts"
    [ good "Init script (Guess game)" initGuessGame
    , good "Spend script (Guess game)" makeGuessGame
    , bad "Bad guess" badGuessGame
    ]
  where
    good = check True
    bad = check False
    check res msg act = testCase msg $ fst (runBch act (initBch cfg $ adaValue 10_000_000)) @?= res

initGuessGame :: Run Bool
initGuessGame = do
  users <- setupUsers
  let u1 = head users
      answer = "secret"
      prize = adaValue 100
  initGame u1 prize answer
  isOk <- noErrors
  val1 <- valueAt u1
  gameVal <- valueAt gameAddress
  gameUtxos <- utxoAt gameAddress
  let [(gameRef, gameOut)] = gameUtxos
  mDat <- datumAt @GameDatum gameRef
  pure $
    and
      [ isOk
      , val1 == adaValue 900
      , gameVal == prize
      , txOutValue gameOut == prize
      , mDat == Just (GuessHash $ Plutus.sha2_256 answer)
      ]

badGuessGame :: Run Bool
badGuessGame = makeGuessGameBy gameSecret "bad guess"

makeGuessGame :: Run Bool
makeGuessGame = makeGuessGameBy gameSecret gameSecret

gameSecret :: BuiltinByteString
gameSecret = "secret"

makeGuessGameBy :: BuiltinByteString -> BuiltinByteString -> Run Bool
makeGuessGameBy secret answer = do
  users <- setupUsers
  let [u1, u2, _] = users
  initGame u1 (adaValue 100) secret
  postedTx <- guess u2 answer
  vals <- mapM valueAt users
  let [v1, v2, _] = vals
  isOk <- noErrors
  pure $ postedTx && isOk && v1 == adaValue 900 && v2 == adaValue 1100

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
    , payToScript gameScript (GuessHash $ Plutus.sha2_256 answer) val
    ]

guess :: PubKeyHash -> BuiltinByteString -> Run Bool
guess pkh answer = do
  utxos <- utxoAt gameAddress
  let [(gameRef, gameOut)] = utxos
  mDat <- datumAt @GameDatum gameRef
  case mDat of
    Just dat -> checkBalance (gives gameScript (txOutValue gameOut) pkh) $ do
      tx <- signTx pkh $ guessTx pkh gameRef (txOutValue gameOut) dat answer
      isRight <$> sendTx tx
    Nothing -> pure False

guessTx :: PubKeyHash -> TxOutRef -> Value -> GameDatum -> BuiltinByteString -> Tx
guessTx pkh gameRef gameVal dat answer =
  mconcat
    [ spendScript gameScript gameRef (Guess answer) dat
    , payToPubKey pkh gameVal
    ]
