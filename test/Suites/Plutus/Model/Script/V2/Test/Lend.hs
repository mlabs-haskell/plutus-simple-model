-- | Test for forward to redeemer. We delegate minting to the spending
-- of script with certain redeemer.
module Suites.Plutus.Model.Script.V2.Test.Lend (
  tests,
) where

import Prelude

import Test.Tasty

import Plutus.V1.Ledger.Value (tokenName)
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude qualified as Plutus
import Suites.Plutus.Model.Script.V2.Onchain.Lend
import Suites.Plutus.Model.Script.V2.Onchain.Lend.Script
import Suites.Plutus.Model.Util
import Plutus.Model.Ada (Ada(..))
import Plutus.Model.Ada qualified as Ada

import Plutus.Model

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Lend scripts (forward to redeemer)"
    [ good "Make exchange" makeExchange
    , good "Mint with no exchange" mintNoExchange
    , good "Buy more than sell" buyMoreThanSell
    ]
  where
    good msg act = testNoErrors (adaValue 10_000_000) cfg msg act

-------------------------------------------------------
-- scripts

setupLend :: Run (App, [PubKeyHash])
setupLend = do
  (app, users@(owner : _)) <- initApp
  initLend app owner
  pure (app, users)

makeExchange :: Run ()
makeExchange = do
  (app, _owner : u1 : _users) <- setupLend
  sell app u1 (Lovelace 100)
  buy app u1 50
  buy app u1 50

mintNoExchange :: Run ()
mintNoExchange = do
  (app, _owner : u1 : _users) <- setupLend
  mustFail $ stealTokens app u1 100

buyMoreThanSell :: Run ()
buyMoreThanSell = do
  (app, _owner : u1 : _users) <- setupLend
  sell app u1 (Lovelace 100)
  mustFail $ buy app u1 2000

-------------------------------------------------------
-- API

data App = App
  { app'lendScript :: Lend
  , app'lendMint   :: LendMint
  , app'lendSymbol :: CurrencySymbol
  , app'lendToken  :: TokenName
  , app'lendValue  :: Integer -> Value
  }

newApp :: App
newApp = App
  { app'lendScript = lendScript
  , app'lendMint   = policy
  , app'lendSymbol = sym
  , app'lendToken  = tok
  , app'lendValue  = singleton sym tok
  }
  where
    sym = scriptCurrencySymbol policy
    tok = tokenName "ExchangeToken"
    policy = lendPolicy lendMintParams
    lendMintParams = LendMintParams (LendHash $ validatorHash lendScript)

withLend :: App -> (TxBox Lend -> Run ()) -> Run ()
withLend App{..} = withNft app'lendScript

initApp :: Run (App, [PubKeyHash])
initApp = do
  users <- setupUsers
  let app = newApp
  pure (app, users)

initLend :: App -> PubKeyHash -> Run ()
initLend App{..} pkh = do
  usp <- spend pkh riderAda
  submitTx pkh $ mconcat
    [ userSpend usp
    , payToScript app'lendScript (HashDatum $ LendDatum app'lendSymbol app'lendToken) riderAda
    ]

sell :: App -> PubKeyHash -> Ada -> Run ()
sell app@App{..} user amount = do
  withLend app $ \lendBox ->
    withSpend user (Ada.toValue amount <> riderAda) $ \usp ->
      submitTx user $ sellTx usp lendBox
  where
    sellTx usp lendBox =
      mconcat
        [ userSpend usp
        , mintValue app'lendMint () mintVal
        , modifyBox app'lendScript lendBox Exchange HashDatum ( <> Ada.toValue amount)
        , payToKey user (riderAda <> mintVal)
        ]

    mintVal = app'lendValue $ Ada.getLovelace amount

buy :: App -> PubKeyHash -> Integer -> Run ()
buy app@App{..} user amount = do
  withLend app $ \lendBox ->
    withSpend user mintVal $ \usp ->
      submitTx user $ buyTx usp lendBox
  where
    buyTx usp lendBox =
      mconcat
        [ userSpend usp
        , mintValue app'lendMint () (Plutus.negate mintVal)
        , modifyBox app'lendScript lendBox Exchange HashDatum ( <> Plutus.negate userVal)
        , payToKey user userVal
        ]

    userVal = Ada.lovelaceValue amount
    mintVal = app'lendValue amount

stealTokens :: App -> PubKeyHash -> Integer -> Run ()
stealTokens App{..} user amount = do
  usp <- spend user riderAda
  submitTx user $ stealTx usp
  where
    stealTx usp =
      mconcat
        [ userSpend usp
        , mintValue app'lendMint () mintVal
        , payToKey user mintVal
        ]

    mintVal = app'lendValue amount

