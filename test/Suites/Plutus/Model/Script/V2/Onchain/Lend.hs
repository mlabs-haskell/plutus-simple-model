{- | Onchain code for exchange platform.

 User can sell tokens on the scripts for tokens for Ada and buy the Ada back.
 It tests minting policy that forwards to script that is spend with certain redeemer.
-}
module Suites.Plutus.Model.Script.V2.Onchain.Lend (
  LendDatum (..),
  LendAct (..),
  LendHash (..),
  LendMintParams (..),
  lendContract,
  lendPolicyContract,
) where

import Plutus.Model.V2
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import PlutusTx qualified
import PlutusTx.Prelude

data LendDatum = LendDatum
  { lendDatum'symbol :: CurrencySymbol
  , lendDatum'token :: TokenName
  }

data LendAct = Exchange

PlutusTx.unstableMakeIsData ''LendAct

{-# INLINEABLE lendContract #-}
lendContract :: LendDatum -> LendAct -> ScriptContext -> Bool
lendContract (LendDatum sym tok) Exchange ctx =
  traceIfFalse
    "Value exhange preserved"
    (getLovelace (adaOf outVal) - getLovelace (adaOf inVal) == mintedAmount)
    && traceIfFalse
      "Same datum"
      (txOutDatum tin == txOutDatum tout)
  where
    info = scriptContextTxInfo ctx
    (tin, tout) = getThrough ctx
    inVal = txOutValue tin
    outVal = txOutValue tout
    mintedAmount = valueOf (txInfoMint info) sym tok

newtype LendHash = LendHash ScriptHash
newtype LendMintParams = LendMintParams LendHash

{-# INLINEABLE lendPolicyContract #-}
lendPolicyContract :: LendMintParams -> () -> ScriptContext -> Bool
lendPolicyContract (LendMintParams (LendHash lendVh)) _ ctx =
  traceIfFalse
    "Lend contract is spent with Exchange redeemer"
    (forwardTo lendVh Exchange info)
  where
    info = scriptContextTxInfo ctx

PlutusTx.unstableMakeIsData ''LendDatum
PlutusTx.makeLift ''LendHash
PlutusTx.makeLift ''LendMintParams
