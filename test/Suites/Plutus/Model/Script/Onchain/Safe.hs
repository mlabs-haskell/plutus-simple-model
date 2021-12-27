{- | Safe contract. User can create safe to freeze value spending until certain time.
 After safe is created user can add values before time to spend has come and
 can spend it only when time is due.
-}
module Suites.Plutus.Model.Script.Onchain.Safe (
  Safe,
  SafeDatum (..),
  SafeAct (..),
  SafeParams (..),
  safeContract,
  safeScript,
  safeValidator,
  safeAddress,
) where

import Prelude

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts (
  findDatum,
  findOwnInput,
  getContinuingOutputs,
  txSignedBy,
 )
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V1.Ledger.Value (gt)
import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

data Safe

data SafeDatum = Safe PubKeyHash
  deriving (Eq)

instance Plutus.Eq SafeDatum where
  Safe pkh1 == Safe pkh2 = pkh1 Plutus.== pkh2

data SafeAct = Spend | Deposit

data SafeParams = SafeParams POSIXTime

instance Scripts.ValidatorTypes Safe where
  type DatumType Safe = SafeDatum
  type RedeemerType Safe = SafeAct

{-# INLINEABLE safeContract #-}
safeContract :: SafeParams -> SafeDatum -> SafeAct -> Ledger.ScriptContext -> Bool
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
      to _spendTime `contains` txInfoValidRange info
        && txOutValue ownOutput `gt` txOutValue ownInput

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

-- | The GeroGov validator script instance
safeScript :: SafeParams -> Scripts.TypedValidator Safe
safeScript sp =
  Scripts.mkTypedValidator @Safe
    ( $$(PlutusTx.compile [||safeContract||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @SafeDatum @SafeAct

-- | The validator of the GeroGov script
safeValidator :: SafeParams -> Scripts.Validator
safeValidator = Scripts.validatorScript . safeScript

-- | The script address of the GeroGov script
safeAddress :: SafeParams -> Ledger.Address
safeAddress = Ledger.scriptAddress . safeValidator

PlutusTx.unstableMakeIsData ''SafeDatum
PlutusTx.unstableMakeIsData ''SafeAct
PlutusTx.unstableMakeIsData ''SafeParams
PlutusTx.makeLift ''SafeParams
