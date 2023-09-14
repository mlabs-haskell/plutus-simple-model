{-# LANGUAGE QualifiedDo #-}

module Suites.Plutarch (tests) where

import Plutarch.Prelude
import Prelude

import Cardano.Simple.PlutusLedgerApi.V1.Scripts (applyPlutarchTypedScript, mkPlutarchTypedScript)
import Control.Monad (void)
import Data.Default (def)
import Data.Either (fromRight)
import Plutarch.Api.V2 (PValidator)
import Plutarch.Monadic qualified as P
import Plutus.Model (DatumMode (..), Run (..), TypedValidator (..), adaValue, defaultBabbageV2, mustFail, newUser, payToKey, payToScript, sendTx, signTx, spend, spendScript, userSpend, utxoAt)
import Plutus.Model.Contract (testNoErrors)
import Plutus.Model.V2 (mkTypedValidatorPlutarch, mkTypedValidatorPlutarchTypedScript)
import PlutusLedgerApi.V1 (toBuiltin)
import PlutusLedgerApi.V2 (BuiltinByteString)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Plutarch"
    [ testGroup
        "Game version with datums"
        [ good "Good guess" (testValidatorWithDatum "aa")
        , bad "Bad guess" (testValidatorWithDatum "bb")
        ]
    , testGroup
        "Game version with parametrized validator"
        [ good "Good guess" (testValidatorParametrized "aa")
        , bad "Bad guess" (testValidatorParametrized "bb")
        ]
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg
    cfg = defaultBabbageV2

pHash :: Term s PByteString
pHash = phexByteStr "961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506"

testValidator :: GameValidator -> BuiltinByteString -> Run ()
testValidator validator guess = do
  u1 <- newUser (adaValue 1_000_000)
  u2 <- newUser (adaValue 1_000_000)
  let amt = adaValue 100
      -- magic string is from 'echo -n aa | sha256sum'
      datum = toBuiltin $ plift pHash
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

testValidatorWithDatum :: BuiltinByteString -> Run ()
testValidatorWithDatum = testValidator validatorWithDatum

testValidatorParametrized :: BuiltinByteString -> Run ()
testValidatorParametrized = testValidator validatorParametrized

type GameValidator = TypedValidator BuiltinByteString BuiltinByteString

validatorWithDatum :: GameValidator
validatorWithDatum =
  fromRight (error "no validator") $
    mkTypedValidatorPlutarch def gameContractPlutarchWithDatum

validatorParametrized :: GameValidator
validatorParametrized =
  fromRight (error "no validator") $ do
    -- Execution time of `mkTypedValidatorPlutarch` and `mkPlutarchTypedScript`
    -- can take up to 2 seconds for large validators.
    -- One definitely should avoid calling these functions in every test.
    -- It's recommended to compile a validator exactly once
    -- to keep low psm tests execution time.
    compiledParametrizedScript <-
      mkPlutarchTypedScript def gameContractPlutarchParametrized

    -- Once we have access to compiled (parametrized) validator script
    -- we can apply it to some parameters which is usually very cheap.
    -- This can be done in every test -
    -- especially if validator's parameter varies across tests.
    validator <-
      applyPlutarchTypedScript def compiledParametrizedScript pHash

    pure $ mkTypedValidatorPlutarchTypedScript validator

-- Datum is used to submit a hash of a puzzle
gameContractPlutarchWithDatum :: ClosedTerm PValidator
gameContractPlutarchWithDatum = plam $ \d' r' _ -> popaque $ P.do
  (r :: Term s (PAsData PByteString), _) <- ptryFrom r'
  (d :: Term s (PAsData PByteString), _) <- ptryFrom d'
  pif
    (psha2_256 # pfromData r #== pfromData d)
    (pconstant ())
    perror

-- Script is parametrized by a hash of a puzzle; Datum does not matter
-- The hash is set once and for all.
-- For a new game the term below needs to be applied to a new hash.
gameContractPlutarchParametrized :: ClosedTerm (PByteString :--> PValidator)
gameContractPlutarchParametrized = plam $ \expectedHash _ r' _ -> popaque $ P.do
  (r :: Term s (PAsData PByteString), _) <- ptryFrom r'
  pif
    (psha2_256 # pfromData r #== expectedHash)
    (pconstant ())
    perror
