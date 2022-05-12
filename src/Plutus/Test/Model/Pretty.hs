{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Pretty printers for blockchain state
module Plutus.Test.Model.Pretty(
  ppPercent,
  ppStatPercent,
  ppLimitInfo,
  ppBlockchain,
  ppFailure,
  ppBalanceWith,
) where

import Prelude
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Foldable (toList)
import Text.Printf (printf)
import Prettyprinter

import Cardano.Api.Shelley (Error (..))
import Ledger (txId)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Slot
import Plutus.V1.Ledger.Value (assetClass, flattenValue, toString)

import Plutus.Test.Model.Blockchain
import Plutus.Test.Model.Fork.TxExtra
import Plutus.Test.Model.Stake (DCertError(..), WithdrawError(..))

-- | Pretty-print for stats experessed in percents
ppStatPercent :: StatPercent -> String
ppStatPercent = show . pretty

ppPercent :: Percent -> String
ppPercent (Percent x) = printf "%.2f" x <> "%"

ppLimitInfo :: BchNames -> Log BchEvent -> String
ppLimitInfo names bch =
  show $ vcat $ fmap ppGroup $ fromGroupLog bch
  where
    ppGroup (slot, events) = vcat [ pretty slot <> colon, indent 2 (vcat $ fmap ppStatEvent events)]

    ppStatEvent = \case
      BchInfo msg -> pretty msg
      BchTx tx    -> vcat [ ppTx $ Ledger.txId $ tx'plutus $ txStatTx tx
                          , indent 2 $ ppStatWarn (txStatPercent tx)
                          ]
      BchFail err ->
        case err of
          TxLimitError _ _ -> mempty
          other            -> pretty other

    ppTx ident = "Tx name/id:" <+> case readTxName names ident of
      Just name -> pretty name
      Nothing -> pretty ident

    ppStatWarn stat
      | isLimitError stat = vcat ["error: out of limits", indent 2 (pretty stat)]
      | otherwise         = pretty stat

    isLimitError (StatPercent size units) =
      err size || err (percentExecutionMemory units) || err (percentExecutionSteps units)
      where
        err (Percent x) = x >= 100

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
    [ "Balances:"
    , indent 2 . vcat $
        prettyBalances pubKeyAddrs <>
        prettyBalances scriptAddrs
    , "Current slot:" <+> pretty (getSlot $ bchCurrentSlot bch)
    , case toList . unLog $ bchFails bch of
       [] -> mempty
       xs -> vcat [ "Failures:"
                  , indent 2 . vcat . map (ppFailureWith names) $ xs
                  ]
    ]
   where
    names = bchNames bch
    valueSet =
      foldMap $ maybe mempty txOutValue . (\ref -> M.lookup ref (bchUtxos bch))

    prettyBalances =
      fmap balance . L.sortBy (\(a,_) (b,_) -> compare a b) . fmap toAddrName

    isPubKeyCredential (addr,_) = case addressCredential addr of
                                    PubKeyCredential _ -> True
                                    _ -> False

    (pubKeyAddrs, scriptAddrs) =
      L.partition isPubKeyCredential $ M.toList (bchAddresses bch)

    toAddrName (addr, txSet) =
      case readAddressName names addr of
        Just addrName -> (addrName, txSet)
        Nothing -> (show . pretty $ addr, txSet)

    balance (prettyAddr, txSet) = vcat
      [ pretty prettyAddr <> colon
      , indent 2 (ppBalanceWith names (valueSet txSet))
      ]

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
    BchInfo msg     -> "[info]  " <+> pretty msg
    BchTx _         -> "[tx]    " <+> "TODO print tx"
    BchFail fReason -> "[error] " <+> pretty fReason

instance Pretty WithdrawError where
  pretty = \case
    WithdrawError cred expected actual ->
      hsep [ "Wrong amount of withdraw for staking credential", pretty cred, "expected", pretty expected
           , "but actual", pretty actual
           ]
    WithdrawNotSigned pkh -> hsep ["Reward withdraw by pub key not signed by", pretty pkh]
    StakeNotRegistered cred ->
      hsep [ "Stake credential", pretty cred, "is not registered for rewards"]
