{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Pretty printers for blockchain state
module Plutus.Test.Model.Pretty(
  ppBalanceSheet,
  ppBalanceWith,
  ppBchEvent,
  ppBlockchain,
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
import Prelude
import Prettyprinter
import Text.Printf (printf)

import Cardano.Api.Shelley (Error (..))
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (assetClass, flattenValue, toString)
import Ledger (Slot (..))

import Plutus.Test.Model.Blockchain
import Plutus.Test.Model.Fork.TxExtra
import Plutus.Test.Model.Stake (DCertError(..), WithdrawError(..))

-- | Pretty-print for stats experessed in percents
ppStatPercent :: StatPercent -> String
ppStatPercent = show . pretty

ppPercent :: Percent -> String
ppPercent (Percent x) = printf "%.2f" x <> "%"

ppBchEvent :: BchNames -> Log BchEvent -> String
ppBchEvent _names = show . vcat . fmap ppSlot . fromGroupLog
  where
    ppSlot (slot, events) = vcat [pretty slot <> colon, indent 2 (vcat $ pretty <$> events)]

ppLimitInfo :: BchNames -> Log TxStat -> String
ppLimitInfo names bch =
  show $ vcat $ fmap ppGroup $ fromGroupLog bch
  where
    ppGroup (slot, events) =
      vcat [ pretty slot <> colon
           , indent 2 (vcat $ fmap ppEvent events)
           ]

    ppEvent event =
      vcat [ ppTxIdent (txStatId event)
           , indent 2 $ pretty event
           ]

    ppTxIdent ident = "Tx name/id:" <+>
      maybe (pretty ident) pretty (readTxName names ident)

-- | Pretty-prints the blockchain state
ppBlockchain :: Blockchain -> String
ppBlockchain bch = show $ pretty bch

-- | Pretty-prints a failure
ppFailure :: BchNames -> (Slot, FailReason) -> String
ppFailure names fails = show $ ppFailureWith names fails

ppFailureWith :: BchNames -> (Slot, FailReason) -> Doc ann
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
            Just name -> vcat [ pretty name <+> "doesn't have enough funds to pay:"
                              , indent 5 (ppBalanceWith names val)
                              ]
            Nothing -> pretty fReason
        _ -> pretty fReason

ppTransaction :: Tx -> String
ppTransaction = show . pretty

ppBalanceWith :: BchNames -> Value -> Doc ann
ppBalanceWith names val = vcat $ fmap
  (\(cs, tn, amt) ->
    (if cs == adaSymbol && tn == adaToken
     then "ADA"
     else
       case readAssetClassName names (assetClass cs tn) of
           Just acName -> pretty acName
           Nothing     -> case readCurrencySymbolName names cs of
                            Just csName -> pretty csName
                            Nothing -> pretty cs
                           <> comma <+> pretty (toString tn)
    ) <> colon <+> pretty amt
    )
    (flattenValue val)

instance Pretty StatPercent where
  pretty (StatPercent size units) = vcat
    [ "size  : " <> pretty size
    , "mem   : " <> pretty (percentExecutionMemory units)
    , "steps : " <> pretty (percentExecutionSteps units)
    ]

instance Pretty Percent where
  pretty = pretty . ppPercent

instance Pretty Blockchain where
  pretty bch = vcat
      [ prettyBalanceSheet bch
      , "Current slot:" <+> pretty (getSlot $ bchCurrentSlot bch)
      , case toList . unLog $ bchFails bch of
         [] -> mempty
         xs -> vcat [ "Failures:"
                    , indent 2 . vcat . map (ppFailureWith names) $ xs
                    ]
      ]
    where
      names = bchNames bch

type BchUtxos = M.Map TxOutRef TxOut
type Utxos = Set TxOutRef

prettyBalances :: BchNames -> BchUtxos -> [(Address, Utxos)] -> [Doc ann]
prettyBalances names utxos =
  fmap (balance names utxos). L.sortBy (\(a,_) (b,_) -> compare a b) . fmap (toAddrName names)

toAddrName :: BchNames -> (Address, b) -> (String, b)
toAddrName names (addr, txSet) =
  case readAddressName names addr of
    Just addrName -> (addrName, txSet)
    Nothing -> (show . pretty $ addr, txSet)

balance :: BchNames -> BchUtxos -> (String, Utxos) -> Doc ann1
balance names utxos (prettyAddr, txSet) = vcat
  [ pretty prettyAddr <> colon
  , indent 2 (ppBalanceWith names (valueSet utxos txSet))
  ]

valueSet :: BchUtxos -> Utxos -> Value
valueSet utxos = foldMap $ maybe mempty txOutValue . (\ref -> M.lookup ref utxos)

-- | Pretty-prints balance for all users/scripts
ppBalanceSheet :: Blockchain -> String
ppBalanceSheet = show . prettyBalanceSheet

prettyBalanceSheet :: Blockchain -> Doc ann
prettyBalanceSheet bch = vcat
   [ "Balances:"
   , indent 2 . vcat $
       prettyBalances names utxos pubKeyAddrs <>
       prettyBalances names utxos scriptAddrs
   ]
 where
   names = bchNames bch
   utxos = bchUtxos bch

   (pubKeyAddrs, scriptAddrs) =
     L.partition isPubKeyCredential $ M.toList (bchAddresses bch)

   isPubKeyCredential (addr,_) = case addressCredential addr of
                                   PubKeyCredential _ -> True
                                   _ -> False

instance Pretty FailReason where
  pretty = \case
    NoUser pkh ->
      "User with PubKeyHash" <+> pretty (getPubKeyHash pkh) <+> "not found"
    NotEnoughFunds pkh val -> vcat
      [ "User with PubKeyHash" <+> pretty (getPubKeyHash pkh)
        <+> "doesn't have enough funds to pay:"
      , indent 5 (ppBalanceWith (BchNames M.empty M.empty M.empty M.empty M.empty) val)
      ]
    IntervalError err -> "Time or vlaid range related error:" <+> pretty (displayError err)
    NotBalancedTx -> "Not balanced transaction"
    FailToReadUtxo -> "UTXO not found"
    FailToCardano err -> "Failed to convert transaction from Plutus to Cardano:" <+> pretty err
    TxScriptFail errs -> "Script execution error:" <+>
      hsep (punctuate comma $ fmap (pretty . displayError) errs)
    TxInvalidRange _ range -> "Not in valid range" <+> pretty range
    TxLimitError ovfs _ -> hsep (punctuate comma (fmap ppOverflow ovfs))
    TxInvalidWithdraw err -> pretty err
    TxInvalidCertificate cert -> pretty cert
    GenericFail str -> "Generic fail:" <+> pretty str
    where
      ppOverflow (TxSizeError _ pcnt) =
        "Transaction size exceeds the limit by" <+> pretty pcnt
      ppOverflow (ExMemError _ pcnt) =
        "Memory limit exceeds the limit by" <+> pretty pcnt
      ppOverflow (ExStepError _ pcnt) =
        "Execution steps exceed the limit by" <+> pretty pcnt

instance Pretty DCertError where
  pretty = \case
    RegStakeError cred      -> "Failed to register staking credential (already exists)" <+> pretty cred
    DeRegStakeError cred    -> "Failed to deregister staking credential" <+> pretty cred
    DelegateError cred pkh  -> hsep ["Failed to delegate staking credential", pretty cred, "to pool", pretty pkh]
    PoolRegError pkh        -> "Failed to register pool" <+> pretty pkh
    PoolRetireError pkh     -> "Failed to retire pool" <+> pretty pkh
    CertGenesisNotSupported -> "Genesis certificate not supported"
    CertMirNotSupported     -> "Mir certificate not supported"

instance Pretty BchEvent where
  pretty = \case
    BchInfo msg                              -> "[info] " <+> align (pretty msg)
    BchTx mbName txStat                      -> "[tx]   " <+> align (ppTxStat mbName txStat)
    BchFail fReason                          -> "[error]" <+> align (pretty fReason)
    BchMustFailLog (MustFailLog msg fReason) -> "[fail] " <+> align (vcat [pretty msg, pretty fReason])
    where
      ppTxStat mbName txStat =
        vcat [ ppTxName mbName (txStatId txStat)
             , indent 2 $ (pretty . TxStatFull) txStat]
      ppTxName mbName txId = maybe
        ("Tx id:" <+> pretty txId)
        (\name -> "Named tx:" <+> pretty name)
        mbName

instance Pretty WithdrawError where
  pretty = \case
    WithdrawError cred expected actual ->
      hsep [ "Wrong amount of withdraw for staking credential", pretty cred, "expected", pretty expected
           , "but actual", pretty actual
           ]
    WithdrawNotSigned pkh -> hsep ["Reward withdraw by pub key not signed by", pretty pkh]
    StakeNotRegistered cred ->
      hsep [ "Stake credential", pretty cred, "is not registered for rewards"]

instance Pretty TxStat where
  pretty TxStat { txStatPercent = stat} = vcat
    [ "Usage statistics:"
    , indent 2 prettyStat
    ]

    where
      prettyStat
        | isLimitError stat = vcat ["error: out of limits", indent 2 (pretty stat)]
        | otherwise         = pretty stat

      isLimitError (StatPercent size units) =
        err size || err (percentExecutionMemory units) || err (percentExecutionSteps units)
        where
          err (Percent x) = x >= 100

-- | The wrapper for full TxStat prettifier
newtype TxStatFull = TxStatFull TxStat

instance Pretty TxStatFull where
  pretty (TxStatFull txStat) =
    let TxStat { txStatTx = Tx { tx'plutus = tx}} = txStat
    in vcat
         [ pretty tx
         , pretty txStat
         ]

-- TODO implement with respect to 'Pretty' law
instance Pretty Tx where
  pretty = viaShow
