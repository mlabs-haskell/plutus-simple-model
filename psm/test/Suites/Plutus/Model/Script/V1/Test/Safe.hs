{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Suites.Plutus.Model.Script.V1.Test.Safe (
  tests,
) where

import Data.Either (isRight)
import Data.Functor (void)
import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.Model
import PlutusLedgerApi.V2
import Suites.Plutus.Model.Script.V1.Onchain.Safe
import Suites.Plutus.Model.Script.V1.Onchain.Safe.Script
import Suites.Plutus.Model.Util

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Safe scripts"
    [ good "Init script (safe)" initSafe
    , good "Spend script (safe)" makeSpendSafe
    , bad "Bad spend (time is not due)" badSpendSafe
    , bad "Bad deposit (time is past)" badDepositSafe
    ]
  where
    good = check True
    bad = check False
    check res msg act = testCase msg $ fst (runMock act (initMock cfg $ adaValue 10_000_000)) @?= res

safe :: Safe
safe = safeScript safeParams

safeParams :: SafeParams
safeParams = SafeParams 1_635_180_185_200

initSafe :: Run Bool
initSafe = do
  users <- setupUsers
  let u1 = head users
      deposit = adaValue 100
  initSafe' u1 deposit
  isOk <- noErrors
  val1 <- valueAt u1
  safeVal <- valueAt safe
  safeUtxos <- utxoAt safe
  let [(safeRef, safeOut)] = safeUtxos
  mDat <- datumAt @SafeDatum safeRef
  pure $
    and
      [ isOk
      , val1 == adaValue 900
      , safeVal == deposit
      , txOutValue safeOut == deposit
      , mDat == Just (Safe u1)
      ]

makeSpendSafe :: Run Bool
makeSpendSafe = waitAndSpendSafe 1_635_180_186_000

badSpendSafe :: Run Bool
badSpendSafe = waitAndSpendSafe 1_635_180_185_000

badDepositSafe :: Run Bool
badDepositSafe = waitAndDepositSafe 1_635_180_186_000

initSafe' :: PubKeyHash -> Value -> Run ()
initSafe' pkh dep = do
  sp <- spend pkh dep
  tx <- signTx pkh $ initSafeTx sp dep pkh
  void $ sendTx tx

waitAndSpendSafe :: POSIXTime -> Run Bool
waitAndSpendSafe timeAhead = spendSafe timeAhead (from timeAhead)

waitAndDepositSafe :: POSIXTime -> Run Bool
waitAndDepositSafe timeAhead = depositSafe timeAhead (to timeAhead)

spendSafe :: POSIXTime -> POSIXTimeRange -> Run Bool
spendSafe timeAhead validInterval = do
  users <- setupUsers
  let u1 = head users
      deposit = adaValue 100
  initSafe' u1 deposit
  waitUntil timeAhead
  utxos <- utxoAt safe
  let [(safeRef, safeOut)] = utxos
      tx = spendTx u1 safeRef (txOutValue safeOut)
  isRight <$> (sendTx =<< signTx u1 =<< validateIn validInterval tx)

depositSafe :: POSIXTime -> POSIXTimeRange -> Run Bool
depositSafe timeAhead validInterval = do
  users <- setupUsers
  let u1 = head users
      deposit = adaValue 100
  initSafe' u1 deposit
  waitUntil timeAhead
  utxos <- utxoAt safe
  sp <- spend u1 deposit
  let [(safeRef, safeOut)] = utxos
      tx = depositTx sp deposit u1 safeRef (txOutValue safeOut)
  isRight <$> (sendTx =<< signTx u1 =<< validateIn validInterval tx)

initSafeTx :: UserSpend -> Value -> PubKeyHash -> Tx
initSafeTx usp val pkh =
  mconcat
    [ userSpend usp
    , payToScript safe (HashDatum $ Safe pkh) val
    ]

spendTx :: PubKeyHash -> TxOutRef -> Value -> Tx
spendTx pkh safeRef safeVal =
  mconcat
    [ spendScript safe safeRef Spend (Safe pkh)
    , payToKey pkh (safeVal <> adaValue (-1))
    , payToScript safe (HashDatum $ Safe pkh) (adaValue 1)
    ]

depositTx :: UserSpend -> Value -> PubKeyHash -> TxOutRef -> Value -> Tx
depositTx sp dep pkh safeRef safeVal =
  mconcat
    [ userSpend sp
    , spendScript safe safeRef Deposit (Safe pkh)
    , payToScript safe (HashDatum $ Safe pkh) (safeVal <> dep)
    ]
