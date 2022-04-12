module Suites.Plutus.Model.Script.Test.Staking (
  tests
) where

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
  let fee = adaValue 10
  sp <- spend u1 fee
  submitTx u1 (withdrawTx u1 u2 sp fee)

withdrawTx :: PubKeyHash -> PubKeyHash -> UserSpend -> Value -> Tx
withdrawTx u1 u2 sp fee =
  mconcat
    [ userSpend sp
    , payFee fee
    , payToPubKey u1 (adaValue 50)
    , payToPubKey u2 (adaValue 50)
    , stakeWithdrawScript (stakeValidator $ toAddress u2) () 100
    ]

