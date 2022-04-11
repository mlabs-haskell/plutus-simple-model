module Suites.Plutus.Model.Script.Test.Staking (
  tests
) where

-- import Data.Either (isRight)
import Data.Functor (void)
import Prelude

import Test.Tasty

import Plutus.Test.Model
import Plutus.V1.Ledger.Api
import Suites.Plutus.Model.Script.Onchain.Staking
import Suites.Plutus.Model.Util

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Staking scripts"
    [ good "Do staking" stakingTest
    ]
  where
    good = testNoErrors initFunds cfg
    initFunds = adaValue 10_000_000

stakingTest :: Run ()
stakingTest = do
  u1 : u2 : _ <- setupUsers
  void $ sendTx =<< withdrawTx u1 u2

withdrawTx :: PubKeyHash -> PubKeyHash -> Run Tx
withdrawTx u1 u2 = do
  sp <- spend u1 fee
  tx <- signTx u1 $ mconcat
    [ userSpend sp
    , payFee fee
    , payToPubKey u1 (adaValue 50)
    , payToPubKey u2 (adaValue 50)
    ]
  pure $ stakeWithdrawScript (stakeValidator $ toAddress u2) () 100 tx
  where
    fee = adaValue 10

