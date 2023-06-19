{-# LANGUAGE QualifiedDo #-}

module Suites.Plutarch (tests) where

import Plutarch.Prelude
import Prelude

import Control.Monad (void)
import Data.Default (def)
import Data.Either (fromRight)
import Plutarch.Api.V2 (PValidator)
import Plutarch.Monadic qualified as P
import Plutus.Model (DatumMode (..), Run (..), TypedValidator (..), adaValue, defaultBabbageV2, mustFail, newUser, payToKey, payToScript, sendTx, signTx, spend, spendScript, userSpend, utxoAt)
import Plutus.Model.Contract (testNoErrors)
import Plutus.Model.V2 (mkTypedValidatorPlutarch)
import PlutusLedgerApi.V1 (toBuiltin)
import PlutusLedgerApi.V2 (BuiltinByteString)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Plutarch"
    [ good "Good guess" (testValidator "aa")
    , bad "Bad guess" (testValidator "bb")
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg
    cfg = defaultBabbageV2

testValidator :: BuiltinByteString -> Run ()
testValidator guess = do
  u1 <- newUser (adaValue 1_000_000)
  u2 <- newUser (adaValue 1_000_000)
  let amt = adaValue 100
      -- magic string is from 'echo -n aa | sha256sum'
      datum =
        toBuiltin $
          plift $
            phexByteStr
              "961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506"
  do
    -- initialize
    sp <- spend u1 amt
    tx <-
      signTx u1 $
        userSpend sp
          <> payToScript validator (InlineDatum datum) amt
    void $ sendTx tx
  do
    -- guess
    [(ref, _out)] <- utxoAt validator
    tx <-
      signTx u2 $
        spendScript validator ref guess datum
          <> payToKey u2 amt
    void $ sendTx tx

validator :: TypedValidator BuiltinByteString BuiltinByteString
validator =
  fromRight (error "no validator") $
    mkTypedValidatorPlutarch def gameContractPlutarch

gameContractPlutarch :: ClosedTerm PValidator
gameContractPlutarch = plam $ \d' r' _ -> popaque $ P.do
  (r :: Term s (PAsData PByteString), _) <- ptryFrom r'
  (d :: Term s (PAsData PByteString), _) <- ptryFrom d'
  pif
    (psha2_256 # pfromData r #== pfromData d)
    (pconstant ())
    perror
