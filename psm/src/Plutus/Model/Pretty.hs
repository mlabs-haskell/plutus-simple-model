{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pretty printers for blockchain state
module Plutus.Model.Pretty (
  ppBalanceSheet,
  ppBalanceWith,
  ppMockEvent,
  ppMock,
  ppFailure,
  ppLimitInfo,
  ppPercent,
  ppStatPercent,
  ppTransaction,
) where

import Data.Foldable (toList)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Prettyprinter
import Text.Printf (printf)
import Prelude

import Cardano.Simple.Ledger.Slot (Slot (..))
import PlutusLedgerApi.V1.Value (assetClass, flattenValue, toString)
import PlutusLedgerApi.V2

import Cardano.Simple.TxExtra
import Plutus.Model.Mock
import Plutus.Model.Stake (DCertError (..), WithdrawError (..))

-- | Pretty-print for stats experessed in percents
ppStatPercent :: StatPercent -> String
ppStatPercent = show . pretty

ppPercent :: Percent -> String
ppPercent (Percent x) = printf "%.2f" x <> "%"

ppMockEvent :: MockNames -> Log MockEvent -> String
ppMockEvent _names = show . vcat . fmap ppSlot . fromGroupLog
  where
    ppSlot (slot, events) = vcat [pretty slot <> colon, indent 2 (vcat $ pretty <$> events)]

ppLimitInfo :: MockNames -> Log TxStat -> String
ppLimitInfo names mock =
  show $ vcat $ ppGroup <$> fromGroupLog mock
  where
    ppGroup (slot, events) =
      vcat
        [ pretty slot <> colon
        , indent 2 (vcat $ fmap ppEvent events)
        ]

    ppEvent event =
      vcat
        [ ppTxIdent (txStatId event)
        , indent 2 $ pretty event
        ]

    ppTxIdent ident =
      "Tx name/id:"
        <+> maybe (pretty ident) pretty (readTxName names ident)

-- | Pretty-prints the blockchain state
ppMock :: Mock -> String
ppMock mock = show $ pretty mock

-- | Pretty-prints a failure
ppFailure :: MockNames -> (Slot, FailReason) -> String
ppFailure names fails = show $ ppFailureWith names fails

ppFailureWith :: MockNames -> (Slot, FailReason) -> Doc ann
ppFailureWith names (slot, fReason) =
  "Slot" <+> pretty (getSlot slot) <> colon <+> align prettyFailReason
  where
    mUserName = readUserName names
    prettyFailReason =
      case fReason of
        NoUser pkh ->
          case mUserName pkh of
            Just name -> pretty name <+> "not found"
            Nothing -> pretty fReason
        NotEnoughFunds pkh val ->
          case mUserName pkh of
            Just name ->
              vcat
                [ pretty name <+> "doesn't have enough funds to pay:"
                , indent 4 (ppBalanceWith names val)
                ]
            Nothing -> pretty fReason
        NotBalancedTx val ->
          vcat
            [ "Not balanced TX"
            , indent 2 "Got balance diff value:"
            , indent 4 (ppBalanceWith names val)
            ]
        NoMintingPolicy symbols ->
          vcat
            [ "No minting policies script for currency symbols"
            , indent 2 $ vcat $ fmap (ppCurrencySymbolWith names) symbols
            ]
        _ -> pretty fReason

ppTransaction :: Tx -> String
ppTransaction = show . pretty

ppBalanceWith :: MockNames -> Value -> Doc ann
ppBalanceWith names val =
  vcat $
    fmap
      ( \(cs, tn, amt) ->
          ( if cs == adaSymbol && tn == adaToken
              then "ADA"
              else case readAssetClassName names (assetClass cs tn) of
                Just acName -> pretty acName
                Nothing ->
                  ppCurrencySymbolWith names cs
                    <> comma
                    <+> pretty (toString tn)
          )
            <> colon
            <+> pretty amt
      )
      (flattenValue val)

ppCurrencySymbolWith :: MockNames -> CurrencySymbol -> Doc ann
ppCurrencySymbolWith names cs =
  case readCurrencySymbolName names cs of
    Just csName -> pretty csName
    Nothing -> pretty cs

instance Pretty StatPercent where
  pretty (StatPercent size units) =
    vcat
      [ "size  : " <> pretty size
      , "mem   : " <> pretty (percentExecutionMemory units)
      , "steps : " <> pretty (percentExecutionSteps units)
      ]

instance Pretty Percent where
  pretty = pretty . ppPercent

instance Pretty Mock where
  pretty mock =
    vcat
      [ prettyBalanceSheet mock
      , "Current slot:" <+> pretty (getSlot $ mockCurrentSlot mock)
      , case toList . unLog $ mockFails mock of
          [] -> mempty
          xs ->
            vcat
              [ "Failures:"
              , indent 2 . vcat . fmap (ppFailureWith names) $ xs
              ]
      ]
    where
      names = mockNames mock

type MockUtxos = M.Map TxOutRef TxOut
type Utxos = Set TxOutRef

prettyBalances :: MockNames -> MockUtxos -> [(Address, Utxos)] -> [Doc ann]
prettyBalances names utxos =
  fmap (balance names utxos) . L.sortBy (\(a, _) (b, _) -> compare a b) . fmap (toAddrName names)

toAddrName :: MockNames -> (Address, b) -> (String, b)
toAddrName names (addr, txSet) =
  case readAddressName names addr of
    Just addrName -> (addrName, txSet)
    Nothing -> (show . pretty $ addr, txSet)

balance :: MockNames -> MockUtxos -> (String, Utxos) -> Doc ann1
balance names utxos (prettyAddr, txSet) =
  vcat
    [ pretty prettyAddr <> colon
    , indent 2 (ppBalanceWith names (valueSet utxos txSet))
    ]

valueSet :: MockUtxos -> Utxos -> Value
valueSet utxos = foldMap $ maybe mempty txOutValue . (`M.lookup` utxos)

-- | Pretty-prints balance for all users/scripts
ppBalanceSheet :: Mock -> String
ppBalanceSheet = show . prettyBalanceSheet

prettyBalanceSheet :: Mock -> Doc ann
prettyBalanceSheet mock =
  vcat
    [ "Balances:"
    , indent 2 . vcat $
        prettyBalances names utxos pubKeyAddrs
          <> prettyBalances names utxos scriptAddrs
    ]
  where
    names = mockNames mock
    utxos = mockUtxos mock

    (pubKeyAddrs, scriptAddrs) =
      L.partition isPubKeyCredential $ M.toList (mockAddresses mock)

    isPubKeyCredential (addr, _) = case addressCredential addr of
      PubKeyCredential _ -> True
      _ -> False

instance Pretty FailReason where
  pretty = \case
    NoUser pkh ->
      "User with PubKeyHash" <+> pretty (getPubKeyHash pkh) <+> "not found"
    NotEnoughFunds pkh val ->
      vcat
        [ "User with PubKeyHash"
            <+> pretty (getPubKeyHash pkh)
            <+> "doesn't have enough funds to pay:"
        , indent 4 (ppBalanceWith (MockNames M.empty M.empty M.empty M.empty M.empty) val)
        ]
    NotBalancedTx val -> "Not balanced transaction:" <+> pretty val
    FailToReadUtxo -> "UTXO not found"
    FailToCardano err -> "Failed to convert transaction from Plutus to Cardano:" <+> pretty err
    TxInvalidRange _ range -> "Not in valid range" <+> pretty range
    TxLimitError ovfs _ -> hsep (punctuate comma (fmap ppOverflow ovfs))
    TxInvalidWithdraw err -> pretty err
    TxInvalidCertificate cert -> pretty cert
    GenericFail str -> "Generic fail:" <+> pretty str
    NoMintingPolicy symbols -> "No minting policy script for currency symbols:" <+> hcat (fmap pretty symbols)
    where
      ppOverflow (TxSizeError _ pcnt) =
        "Transaction size exceeds the limit by" <+> pretty pcnt
      ppOverflow (ExMemError _ pcnt) =
        "Memory limit exceeds the limit by" <+> pretty pcnt
      ppOverflow (ExStepError _ pcnt) =
        "Execution steps exceed the limit by" <+> pretty pcnt

instance Pretty DCertError where
  pretty = \case
    RegStakeError cred -> "Failed to register staking credential (already exists)" <+> pretty cred
    DeRegStakeError cred -> "Failed to deregister staking credential" <+> pretty cred
    DelegateError cred pkh -> hsep ["Failed to delegate staking credential", pretty cred, "to pool", pretty pkh]
    PoolRegError pkh -> "Failed to register pool" <+> pretty pkh
    PoolRetireError pkh -> "Failed to retire pool" <+> pretty pkh
    CertGenesisNotSupported -> "Genesis certificate not supported"
    CertMirNotSupported -> "Mir certificate not supported"

instance Pretty MockEvent where
  pretty = \case
    MockInfo msg -> "[info] " <+> align (pretty msg)
    MockTx mbName txStat -> "[tx]   " <+> align (ppTxStat mbName txStat)
    MockFail fReason -> "[error]" <+> align (pretty fReason)
    MockMustFailLog (MustFailLog msg fReason) -> "[fail] " <+> align (vcat [pretty msg, pretty fReason])
    where
      ppTxStat mbName txStat =
        vcat
          [ ppTxName mbName (txStatId txStat)
          , indent 2 $ (pretty . TxStatFull) txStat
          ]
      ppTxName mbName txId =
        maybe
          ("Tx id:" <+> pretty txId)
          (\name -> "Named tx:" <+> pretty name)
          mbName

instance Pretty WithdrawError where
  pretty = \case
    WithdrawError cred expected actual ->
      hsep
        [ "Wrong amount of withdraw for staking credential"
        , pretty cred
        , "expected"
        , pretty expected
        , "but actual"
        , pretty actual
        ]
    WithdrawNotSigned pkh -> hsep ["Reward withdraw by pub key not signed by", pretty pkh]
    StakeNotRegistered cred ->
      hsep ["Stake credential", pretty cred, "is not registered for rewards"]

instance Pretty TxStat where
  pretty TxStat {txStatPercent = stat} =
    vcat
      [ "Usage statistics:"
      , indent 2 prettyStat
      ]
    where
      prettyStat
        | isLimitError stat = vcat ["error: out of limits", indent 2 (pretty stat)]
        | otherwise = pretty stat

      isLimitError (StatPercent size units) =
        err size || err (percentExecutionMemory units) || err (percentExecutionSteps units)
        where
          err (Percent x) = x >= 100

-- | The wrapper for full TxStat prettifier
newtype TxStatFull = TxStatFull TxStat

instance Pretty TxStatFull where
  pretty (TxStatFull txStat) =
    -- let TxStat { txStatTx = Tx { tx'plutus = tx}} = txStat
    -- in
    vcat
      [ {- pretty tx -} "pretty TX"
      , pretty txStat
      ]

-- TODO implement with respect to 'Pretty' law
instance Pretty Tx where
  pretty = viaShow
