{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Suites.Plutus.Model.Script.V1.Test.Nft (
  tests,
) where

import Data.Either (isRight)
import Data.Functor (void)
import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.Model
import PlutusLedgerApi.V2
import Suites.Plutus.Model.Script.V1.Onchain.Nft
import Suites.Plutus.Model.Util

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Nft scripts"
    [ good "Mint nft and send it to other user" initNft
    , bad "Bad mint (Tx does not contain ref)" noRefMint
    , bad "Bad mint (wrong amount minted)" wrongAmountMint
    ]
  where
    good = check True
    bad = check False
    check res msg act = testCase msg $ fst (runMock act (initMock cfg $ adaValue 10_000_000)) @?= res

initNft :: Run Bool
initNft = do
  users <- setupUsers
  let [u1, u2, _] = users
  sp <- spend u1 (adaValue 1)
  let params = NftParams (getHeadRef sp) tn
  tx <- signTx u1 $ nftTx params sp u2
  isRight <$> sendTx tx

noRefMint :: Run Bool
noRefMint = do
  users <- setupUsers
  let [u1, u2, _] = users
  sp1 <- spend u1 (adaValue 1000)
  let params = NftParams (getHeadRef sp1) tn
  tx1 <- signTx u1 $ userSpend sp1 <> payToKey u1 (adaValue 1000)
  void $ sendTx tx1
  sp2 <- spend u2 (adaValue 1)
  tx2 <- signTx u1 $ nftTx params sp2 u2
  isRight <$> sendTx tx2

wrongAmountMint :: Run Bool
wrongAmountMint = do
  users <- setupUsers
  let [u1, u2, _] = users
  sp <- spend u1 (adaValue 1)
  let params = NftParams (getHeadRef sp) tn
      val = nftValue params
  tx <- signTx u1 $ nftTxWith (val <> val) params sp u2
  isRight <$> sendTx tx

nftTx :: NftParams -> UserSpend -> PubKeyHash -> Tx
nftTx p sp pkh =
  let val = nftValue p
   in nftTxWith val p sp pkh

nftTxWith :: Value -> NftParams -> UserSpend -> PubKeyHash -> Tx
nftTxWith mintVal p sp pkh =
  let mp = nftMintingPolicy p
   in mconcat
        [ mintValue mp () mintVal
        , userSpend sp
        , payToKey pkh (adaValue 1 <> mintVal)
        ]

tn :: TokenName
tn = TokenName "MLABS"
