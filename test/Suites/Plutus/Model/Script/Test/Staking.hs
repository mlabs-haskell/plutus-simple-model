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
  let fee1 = adaValue 100
      stakeScript = stakeValidator $ toAddress u2
  pool <- head <$> getPools
  sp1 <- spend u1 fee1
  submitTx u1 $ registerCredentialTx stakeScript pool sp1 fee1
  let fee2 = adaValue 10
  sp2 <- spend u1 fee2
  submitTx u1 (withdrawTx stakeScript u1 u2 sp2 fee2)

withdrawTx :: TypedStake () -> PubKeyHash -> PubKeyHash -> UserSpend -> Value -> Tx
withdrawTx stakeScript u1 u2 sp fee =
  mconcat
    [ userSpend sp
    , payFee fee
    , payToPubKey u1 (adaValue 25)
    , payToPubKey u2 (adaValue 25)
    , withdrawStakeScript stakeScript () 50
    ]

registerCredentialTx :: TypedStake () -> PoolId -> UserSpend -> Value -> Tx
registerCredentialTx stakeScript poolId sp fee =
  mconcat
    [ userSpend sp
    , payFee fee
    , registerStakeScript stakeScript ()
    , delegateStakeScript stakeScript () poolId
    ]

