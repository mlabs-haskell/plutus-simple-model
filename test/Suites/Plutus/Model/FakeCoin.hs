{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Can use fake coins
module Suites.Plutus.Model.FakeCoin (
  tests,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2

import Plutus.Model

tests :: MockConfig -> TestTree
tests cfg = good "Simple fake coin exchange" simpleSpend
  where
    good = check True
    check res msg act = testCase msg $ fst (runMock act (initMock cfg $ adaValue 10_000_000 <> dollar 10000 <> euro 10000 <> lira 10000)) @?= res

dollar, euro, lira :: Integer -> Value
dollar = fakeValue (FakeCoin "dollar")
euro = fakeValue (FakeCoin "euro")
lira = fakeValue (FakeCoin "lira")

setupFakeCoinUsers :: Run [PubKeyHash]
setupFakeCoinUsers = do
  u1 <- newUser (adaValue 1000 <> dollar 100)
  u2 <- newUser (adaValue 1000 <> euro 100)
  u3 <- newUser (adaValue 1000 <> lira 100)
  pure [u1, u2, u3]

simpleSpend :: Run Bool
simpleSpend = do
  users <- setupFakeCoinUsers
  let [u1, u2, u3] = users
      -- note that every UTXO should have some ada in it,
      -- that's why we also add a bit of ada on send of every fake coin.
      minAda = adaValue 10
  sendValue u1 (minAda <> dollar 50) u2
  sendValue u2 (minAda <> euro 50) u3
  sendValue u3 (minAda <> lira 50) u1
  isOk <- noErrors
  vals <- mapM valueAt users
  let [v1, v2, v3] = vals
  pure $
    and
      [ isOk
      , v1 == adaValue 1000 <> dollar 50 <> lira 50
      , v2 == adaValue 1000 <> euro 50 <> dollar 50
      , v3 == adaValue 1000 <> lira 50 <> euro 50
      ]
