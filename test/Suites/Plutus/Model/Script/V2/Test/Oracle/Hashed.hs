-- | Test for reference input with hashed datum
module Suites.Plutus.Model.Script.V2.Test.Oracle.Hashed (
  tests,
) where

import Test.Tasty
import Prelude

import Plutus.Model
import PlutusLedgerApi.V2
import Suites.Plutus.Model.Script.V2.Onchain.Oracle
import Suites.Plutus.Model.Script.V2.Onchain.Oracle.Hashed
import Suites.Plutus.Model.Util

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Oracle Bet scripts (reference inputs, hashed datum)"
    [ good "Play bet (Bet game)" playBet
    , good "Wrong user bet" wrongUserBet
    , good "Bet without ref input" betWithoutRefInput
    ]
  where
    good = testNoErrors (adaValue 10_000_000) cfg

-- | Happy path. Two players guess and one of the wins.
playBet :: Run ()
playBet = do
  (app, oracle : _u1 : u2 : _) <- initBetGame
  --  bet app u1 (AnswerDatum 1)
  --  bet app u2 (AnswerDatum 7)
  postAnswer oracle 5
  victory app u2 oracle

-- | Negative path. Two players guess and the wrong one tries to win
wrongUserBet :: Run ()
wrongUserBet = do
  (app, oracle : u1 : u2 : _) <- initBetGame
  bet app u1 (AnswerDatum 1)
  bet app u2 (AnswerDatum 7)
  postAnswer oracle 5
  mustFail $ victory app u1 oracle

{- | Negative path. Two players guess and the right one tries to win but
 omits the oracle reference input.
-}
betWithoutRefInput :: Run ()
betWithoutRefInput = do
  (app, oracle : u1 : u2 : _) <- initBetGame
  bet app u1 (AnswerDatum 1)
  bet app u2 (AnswerDatum 7)
  postAnswer oracle 5
  mustFail $ victorySkipOracle app u2 oracle

--------------------------------------------------------------------
-- API

-- | App holds the scripts of the test suite
newtype App = App
  { app'betScript :: Bet
  }

-- | Init new app with oracle's PKH
newApp :: PubKeyHash -> App
newApp = App . betScript . BetParams

-- | Setups users and application
initBetGame :: Run (App, [PubKeyHash])
initBetGame = do
  users@(oracle : _) <- setupUsers
  let app = newApp oracle
  initBet app oracle
  pure (app, users)

-- | Read bet script box from blockchain
getBet :: App -> Run (TxBox Bet)
getBet App {..} = head <$> boxAt app'betScript

-- | Inits the Bet UTXO with wmpty list of answers.
initBet :: App -> PubKeyHash -> Run ()
initBet App {..} pkh = do
  usp <- spend pkh (adaValue betStep)
  submitTx pkh $ initBetTx usp
  where
    initBetTx usp =
      mconcat
        [ userSpend usp
        , payToScript app'betScript (HashDatum $ BetDatum []) (adaValue betStep)
        ]

-- | User provide san answer as integer and spends @betStep@ of lovelaces.
bet :: App -> PubKeyHash -> AnswerDatum -> Run ()
bet app@App {..} pkh (AnswerDatum answer) = do
  usp <- spend pkh (adaValue betStep)
  betBox <- getBet app
  submitTx pkh $ betTx usp betBox
  where
    betTx usp betBox =
      mconcat
        [ userSpend usp
        , modifyBox app'betScript betBox (Bet answer) (HashDatum . updateDat) updateVal
        ]

    updateDat (BetDatum answers) = BetDatum ((pkh, answer) : answers)
    updateVal = (<> adaValue betStep)

{- | Oracle posts an answer as PubKeyHash guaeded UTXO with inlined datum
 of the answer
-}
postAnswer :: PubKeyHash -> Integer -> Run ()
postAnswer oraclePkh answer = do
  usp <- spend oraclePkh riderAda
  submitTx oraclePkh $
    mconcat
      [ userSpend usp
      , payToKeyDatum oraclePkh (HashDatum (AnswerDatum answer)) riderAda
      ]

-- | Parameters of malicious behavior
newtype Fraud = Fraud
  { fraud'refInput :: Tx -> Tx
  -- ^ tamper refInput part of TX
  }

-- | No malicious actions
noFraud :: Fraud
noFraud = Fraud id

-- | User declares a victory
victory :: App -> PubKeyHash -> PubKeyHash -> Run ()
victory = victoryBy noFraud

-- | User declares a victory but skips the reference input with oracle answer (should fail)
victorySkipOracle :: App -> PubKeyHash -> PubKeyHash -> Run ()
victorySkipOracle =
  victoryBy $
    Fraud
      { fraud'refInput = const mempty
      }

-- | generic action to declare a victory with possible malicious actions
victoryBy :: Fraud -> App -> PubKeyHash -> PubKeyHash -> Run ()
victoryBy Fraud {..} app@App {..} user oracle =
  withUtxo (hasOracleDatum . snd) oracle $ \(oracleRef, _) -> do
    betBox <- getBet app
    withDatum @AnswerDatum oracleRef $ \oracleDatum ->
      submitTx user $ victoryTx oracleRef betBox oracleDatum
  where
    victoryTx oracleRef betBox oracleDatum =
      mconcat
        [ spendBox app'betScript Answer betBox
        , fraud'refInput $ refInputHash oracleRef oracleDatum
        , payToKey user (txBoxValue betBox)
        ]

    hasOracleDatum = isOutputDatumHash . txOutDatum

isOutputDatumHash :: OutputDatum -> Bool
isOutputDatumHash = \case
  OutputDatumHash _ -> True
  _ -> False
