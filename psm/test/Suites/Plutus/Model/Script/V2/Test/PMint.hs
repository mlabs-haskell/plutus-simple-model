module Suites.Plutus.Model.Script.V2.Test.PMint (
  tests,
) where

import Data.Text (unpack)
import Plutarch (Config (..), TracingMode (..))
import Plutarch.Prelude
import Plutus.Model.V2
import PlutusLedgerApi.V2 (PubKeyHash (..), singleton)
import Suites.Plutus.Model.Script.V2.Onchain.PMint (MintParams (..), mkVerifyAuth)
import Test.Tasty (TestTree, testGroup)
import Prelude

tests :: TestTree
tests =
  testGroup
    "Golden tests for minting auth tokens"
    [ validTest "Valid mint tx. Should pass" mintAuthToken defaultAuthContext
    ]

mintAuthToken :: ContextGoldAuth -> Run ()
mintAuthToken _ = do
  equineAdmin <- newUser $ adaValue 1_000_000
  equineBot <- newUser $ adaValue 1_000_000

  withSpend equineAdmin (adaValue 1) $ \sp -> do
    let script = mkAuthPsmScript $ MintParams equineAdmin
    let sc = scriptCurrencySymbol script
    let val = singleton sc "" 100

    tx <-
      signTx equineAdmin $
        mconcat
          [ mintValue script () val
          , userSpend sp
          , payToKey equineBot (adaValue 1 <> val)
          ]
    logOk "tx ready"

    _ <- sendTx tx
    logOk "tx send"

  logOk "Mint is succeeded "

validTest ::
  String ->
  (ContextGoldAuth -> Run a) ->
  ContextGoldAuth ->
  TestTree
validTest mes act context =
  testNoErrorsTrace initFunds defaultBabbageV2 mes (act context)
  where
    initFunds =
      adaValue 1_000_000_000

-- babbageConfig = defaultMockConfig $
--   customBabbageParams $ \ps ->
--     ps
--       { _protocolVersion = ProtVer {pvMajor = 7, pvMinor = 0}
--       }

-- invalidTest ::
--   String ->
--   (ContextGoldAuth -> Run a) ->
--   ContextGoldAuth ->
--   TestTree
-- invalidTest mes = validTest mes . (mustFail .)

newtype ContextGoldAuth = ContextGoldAuth
  { mintParams :: MintParams
  }

defaultAuthContext :: ContextGoldAuth
defaultAuthContext =
  ContextGoldAuth
    { mintParams =
        MintParams $
          PubKeyHash "027a36015d2e79475a18022280671e482a45016360183a083a7c0f22"
    }

type Auth = TypedPolicy ()

mkAuthPsmScript :: MintParams -> Auth
mkAuthPsmScript params =
  case eValidator of
    Left err ->
      error $
        unwords
          [ "Plutarch compilation error:"
          , unpack err
          ]
    Right s -> s
  where
    eValidator =
      mkTypedPolicyPlutarch (Config DoTracing) $
        mkVerifyAuth # pconstant params

logOk :: String -> Run ()
logOk mes = do
  isOK <- noErrors
  logInfo $ mes <> " " <> show isOK
