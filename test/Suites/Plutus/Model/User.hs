-- | Tests creation of users and spending funds
module Suites.Plutus.Model.User (
  tests,
  simpleSpend,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import Data.Functor (void)
import Plutus.Model
import Suites.Plutus.Model.Util
import qualified Suites.Plutus.Model.FakeCoin as FakeCoin

tests :: BchConfig -> TestTree
tests cfg = do
  testGroup
    "Test simple user scripts"
    [ good "Simple spend" simpleSpend
    , bad "Not enough funds" notEnoughFunds
    , FakeCoin.tests cfg
    ]
  where
    good = check True
    bad = check False
    check res msg act = testCase msg $ fst (runBch act (initBch cfg $ adaValue 10_000_000)) @?= res

simpleSpend :: Run Bool
simpleSpend = do
  users <- setupUsers
  let [u1, u2, u3] = users
      val = adaValue 100
  checkBalance (gives u1 val u2) $ sendValue u1 val u2
  checkBalance (gives u2 val u3) $ sendValue u2 val u3
  isOk <- noErrors
  vals <- mapM valueAt users
  pure $ isOk && vals == fmap adaValue [900, 1000, 1100]

notEnoughFunds :: Run Bool
notEnoughFunds = do
  users <- setupUsers
  let [u1, u2, _u3] = users
  void $ sendValue u1 (adaValue 10000) u2
  noErrors
