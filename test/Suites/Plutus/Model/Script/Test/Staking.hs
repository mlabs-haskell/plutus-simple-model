module Suites.Plutus.Model.Script.Test.Staking (
  tests
) where

-- import Data.Either (isRight)
import Data.Functor (void)
import Prelude

import Test.Tasty

import Plutus.Test.Model
import Plutus.V1.Ledger.Api
-- import Plutus.V1.Ledger.Tx (Tx)
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
  void $ sendTx' =<< withdrawTx u1 u2

withdrawTx :: PubKeyHash -> PubKeyHash -> Run TxExtra
withdrawTx u1 u2 = do
  sp <- spend u1 fee
  tx <- fmap toExtra $ signTx u1 $ mconcat
    [ userSpend sp
    , payFee fee
    , payToPubKey u1 (adaValue 50)
    , payToPubKey u2 (adaValue 50)
    ]
  pure $ stakeWithdrawScript (stakeValidator $ toAddress u2) () 100 tx
  where
    fee = adaValue 10


{-
      deposit = adaValue 100
  initSafe' u1 deposit
  isOk <- noErrors
  val1 <- valueAt u1
  safeVal <- valueAt $ safeAddress safeParams
  safeUtxos <- utxoAt $ safeAddress safeParams
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
-}


