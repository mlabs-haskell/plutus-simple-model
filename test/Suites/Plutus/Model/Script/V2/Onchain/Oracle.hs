{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | Oracle example to test reference inputs
 with inlined and hashed datums.

 The players bet on integer value and oracle should provide an aswer.
 Answer is read by reference input. Oracle inlines the answer to datum.

 The BetDatum keeps the track of answers so far. Every user can bet @betStep@ lovelaces
 and append the answer to the list of answers.

 The winner is player with the closest guess to the Oracle's answer.
-}
module Suites.Plutus.Model.Script.V2.Onchain.Oracle (
  AnswerDatum (..),
  BetDatum (..),
  BetAct (..),
  BetParams (..),
  betStep,
  betContract,
) where

import Plutus.Model.V2
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts
import PlutusTx qualified
import PlutusTx.Prelude

-- | Answer of the oracle
newtype AnswerDatum = AnswerDatum Integer

newtype BetDatum = BetDatum [(PubKeyHash, Integer)]

PlutusTx.unstableMakeIsData ''BetDatum

{-# INLINEABLE betStep #-}
betStep :: Integer
betStep = 100

data BetAct
  = -- | User mades a guess
    Bet Integer
  | -- | User answers
    Answer

newtype BetParams = BetParams {betParams'oracle :: PubKeyHash}

{-# INLINEABLE betContract #-}
betContract :: (TxInfo -> TxOut -> Maybe AnswerDatum) -> BetParams -> BetDatum -> BetAct -> ScriptContext -> Bool
betContract readDatum (BetParams oraclePkh) (BetDatum answers) act ctx =
  case act of
    Bet n -> bet n
    Answer -> answer
  where
    !info = scriptContextTxInfo ctx

    bet guess =
      traceIfFalse
        "Answers update is right"
        ((pkh, guess) : answers == answersOut)
        && traceIfFalse
          "Value is increased by bet step"
          (txOutValue tout == txOutValue tin <> singleton adaSymbol adaToken betStep)
      where
        Just (BetDatum answersOut) = datumOf info tout
        (tin, tout) = getThrough ctx
        [pkh] = txInfoSignatories info

    answer =
      traceIfFalse
        "Script is fully spent"
        (null (getContinuingOutputs ctx))
        && traceIfFalse
          "Oracle owner is right"
          (txOutAddress oracleIn == pubKeyHashAddress oraclePkh)
        && traceIfFalse
          "answer is closest of pkh"
          (all (\x -> getAnswerDiff x >= answerDiff) answers)
      where
        [pkh] = txInfoSignatories info
        Just guess = find ((== pkh) . fst) answers
        [oracleInfo] = txInfoReferenceInputs info
        oracleIn = txInInfoResolved oracleInfo
        Just (AnswerDatum oracleAnswer) = readDatum info oracleIn
        answerDiff = getAnswerDiff guess

        getAnswerDiff (_key, n) = abs (oracleAnswer - n)

PlutusTx.unstableMakeIsData ''AnswerDatum
PlutusTx.unstableMakeIsData ''BetAct
PlutusTx.makeLift ''BetParams
