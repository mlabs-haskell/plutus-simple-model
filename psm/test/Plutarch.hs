{-# LANGUAGE QualifiedDo #-}

module Plutarch (tests) where

import Plutarch.Prelude
import Prelude

import Control.Monad (void)
import Data.Default (def)
import Data.Either (fromRight)
import Plutarch.Api.V2 (PValidator)
import Plutarch.Builtin
import Plutus.Model (DatumMode (..), Run (..), TypedValidator (..), adaValue, defaultBabbageV2, mustFail, payToKey, payToScript, sendTx, signTx, spend, spendScript, userSpend, utxoAt)
import Plutus.Model.Contract (testNoErrors)
import Plutus.Model.V2 (mkTypedValidatorPlutarch)
import Suites.Plutus.Model.Util (setupUsers)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Plutarch"
    [ good "Good guess" (testValidator 4)
    , bad "Bad guess" (testValidator 5)
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg
    cfg = defaultBabbageV2

testValidator :: Integer -> Run ()
testValidator guess = do
  [u1, u2, _] <- setupUsers
  let amt = adaValue 100
  do
    -- init
    sp <- spend u1 amt
    tx <-
      signTx u1 $
        userSpend sp
          <> payToScript validator (InlineDatum ()) amt
    void $ sendTx tx
  do
    -- guess
    [(ref, _out)] <- utxoAt validator
    tx <-
      signTx u2 $
        (spendScript validator ref guess ())
          <> payToKey u2 amt
    void $ sendTx tx
  pure ()

validator :: TypedValidator () Integer
validator =
  fromRight (error "no validator") $
    mkTypedValidatorPlutarch def gameContractPlutarch

gameContractPlutarch :: ClosedTerm PValidator
gameContractPlutarch = plam $ \_ r _ -> popaque $ P.do
  pif
    (r #== pforgetData (pdata (4 :: ClosedTerm PInteger)))
    (pconstant ())
    perror
