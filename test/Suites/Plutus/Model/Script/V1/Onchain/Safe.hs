{- | Safe contract. User can create safe to freeze value spending until certain time.
 After safe is created user can add values before time to spend has come and
 can spend it only when time is due.
-}
module Suites.Plutus.Model.Script.V1.Onchain.Safe (
  SafeDatum (..),
  SafeAct (..),
  SafeParams (..),
  safeContract,
) where

import Prelude

import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Contexts (
  findDatum,
  findOwnInput,
  getContinuingOutputs,
  txSignedBy,
 )
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (gt)
import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

newtype SafeDatum = Safe PubKeyHash
  deriving (Eq)

instance Plutus.Eq SafeDatum where
  Safe pkh1 == Safe pkh2 = pkh1 Plutus.== pkh2

PlutusTx.unstableMakeIsData ''SafeDatum

data SafeAct = Spend | Deposit

newtype SafeParams = SafeParams POSIXTime

{-# INLINEABLE safeContract #-}
safeContract :: SafeParams -> SafeDatum -> SafeAct -> ScriptContext -> Bool
safeContract (SafeParams _spendTime) (Safe pkh) act ctx =
  txSignedBy (scriptContextTxInfo ctx) pkh
    && keyIsSame
    && ( case act of
          Spend -> onSpend
          Deposit -> onDeposit
       )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- check that valid range interval is past spendTime
    onSpend :: Bool
    onSpend = from _spendTime `contains` txInfoValidRange info

    -- check that valid range is below interval
    -- and value of script has rised by some amount
    onDeposit :: Bool
    onDeposit =
      to _spendTime
        `contains` txInfoValidRange info
        && txOutValue ownOutput
        `gt` txOutValue ownInput

    -- check that Safe at input and at output has the same pub key hash
    keyIsSame :: Bool
    keyIsSame = (Plutus.== Just True) $ do
      mDh <- txOutDatumHash ownOutput
      mDatum <- findDatum mDh info
      let d = getDatum mDatum
      mSafeDatum <- PlutusTx.fromBuiltinData d
      pure $ mSafeDatum Plutus.== Safe pkh

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Just i -> txInInfoResolved i
      Nothing -> Plutus.error ()

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> Plutus.error ()

PlutusTx.unstableMakeIsData ''SafeAct
PlutusTx.unstableMakeIsData ''SafeParams
PlutusTx.makeLift ''SafeParams
