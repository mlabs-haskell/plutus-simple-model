{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Suites.Plutus.Model.Script.V1.Test.Counter (
  tests,
  initCounter,
  goodCounter,
) where

import Data.Either
import Data.Functor (void)
import Data.Maybe
import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import PlutusLedgerApi.V2
import Suites.Plutus.Model.Script.V1.Onchain.Counter
import Suites.Plutus.Model.Script.V1.Onchain.Counter.Script
import Suites.Plutus.Model.Util

import Plutus.Model

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Counter scripts"
    [ good "Init script (counter)" initCounterTest
    , good "Good increment" goodCounter
    , bad "Bad increment" badCounter
    ]
  where
    check :: String -> Run a -> (a -> Assertion) -> TestTree
    check msg act f = testCase msg $ f $ fst (runMock act (initMock cfg $ adaValue 10_000_000))

    good msg act = check msg (act >> checkErrors) (@?= Nothing)
    bad msg act = check msg (fmap isJust $ act >> checkErrors) (@?= True)

initCounterTest :: Run Bool
initCounterTest = do
  users <- setupUsers
  let u1 = head users
      minAda = adaValue 10
  initCounter u1 minAda
  isOk <- noErrors
  val1 <- valueAt u1
  counterVal <- valueAt counterScript
  counterUtxos <- utxoAt counterScript
  let [(counterRef, counterOut)] = counterUtxos
  mDat <- datumAt @CounterDatum counterRef
  pure $
    and
      [ isOk
      , val1 == adaValue 990
      , counterVal == minAda
      , txOutValue counterOut == minAda
      , mDat == Just (CounterDatum 0)
      ]

badCounter :: Run Bool
badCounter = makeCounterBy badIncrement
  where
    badIncrement = 0

goodCounter :: Run Bool
goodCounter = makeCounterBy 1

makeCounterBy :: Integer -> Run Bool
makeCounterBy inc = do
  u1 <- head <$> setupUsers
  initCounter u1 (adaValue 100)
  postedTx <- increment u1 inc
  v1 <- valueAt u1
  isOk <- noErrors
  pure $ postedTx && isOk && v1 == adaValue 900

initCounter :: PubKeyHash -> Value -> Run ()
initCounter pkh minAda =
  checkBalance (gives pkh minAda counterScript) $ do
    sp <- spend pkh minAda
    tx <- signTx pkh $ initCounterTx sp minAda
    void $ sendTx tx

initCounterTx :: UserSpend -> Value -> Tx
initCounterTx usp minAda =
  mconcat
    [ userSpend usp
    , payToScript counterScript (HashDatum $ CounterDatum 0) minAda
    ]

-- | Increments counter and checks that user and script gathered no new value.
increment :: PubKeyHash -> Integer -> Run Bool
increment pkh inc = checkBalance (pkh `owns` mempty <> counterScript `owns` mempty) $ do
  utxos <- utxoAt counterScript
  let [(counterRef, counterOut)] = utxos
  mDat <- datumAt @CounterDatum counterRef
  case mDat of
    Just dat -> do
      tx <- signTx pkh $ incrementTx counterRef (txOutValue counterOut) dat inc
      isRight <$> sendTx tx
    Nothing -> pure False

incrementTx :: TxOutRef -> Value -> CounterDatum -> Integer -> Tx
incrementTx counterRef counterVal dat inc =
  mconcat
    [ spendScript counterScript counterRef Bump dat
    , payToScript counterScript (HashDatum $ CounterDatum $ getCounterDatum dat + inc) counterVal
    ]
