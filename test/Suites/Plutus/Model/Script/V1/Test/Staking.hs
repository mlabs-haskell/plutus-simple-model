module Suites.Plutus.Model.Script.V1.Test.Staking (
  tests,
) where

import Test.Tasty
import Prelude

import Plutus.Model
import PlutusLedgerApi.V2
import Suites.Plutus.Model.Script.V1.Onchain.Staking
import Suites.Plutus.Model.Util

tests :: MockConfig -> TestTree
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
  let fee1 = Lovelace 100
      stakeScript = stakeValidator $ toAddress u2
  pool <- head <$> getPools
  sp1 <- spend u1 (ada fee1)
  submitTx u1 $ registerCredentialTx stakeScript pool sp1 fee1
  let fee2 = Lovelace 10
  sp2 <- spend u1 (ada fee2)
  submitTx u1 (withdrawTx stakeScript u1 u2 sp2 fee2)

withdrawTx :: TypedStake () -> PubKeyHash -> PubKeyHash -> UserSpend -> Ada -> Tx
withdrawTx stakeScript u1 u2 sp fee =
  mconcat
    [ userSpend sp
    , payFee fee
    , payToKey u1 (adaValue 25)
    , payToKey u2 (adaValue 25)
    , withdrawStakeScript stakeScript () 50
    ]

registerCredentialTx :: TypedStake () -> PoolId -> UserSpend -> Ada -> Tx
registerCredentialTx stakeScript poolId sp fee =
  mconcat
    [ userSpend sp
    , payFee fee
    , registerStakeScript stakeScript
    , delegateStakeScript stakeScript () poolId
    ]
