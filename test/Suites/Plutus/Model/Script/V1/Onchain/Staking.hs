module Suites.Plutus.Model.Script.V1.Onchain.Staking (
  stakeValidator,
) where

import Plutus.Model.V1

import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Value

import PlutusTx qualified
import PlutusTx.Prelude

{-# INLINEABLE mkStakingValidator #-}
mkStakingValidator :: Address -> () -> ScriptContext -> Bool
mkStakingValidator addr () ctx = case scriptContextPurpose ctx of
  Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
  Certifying _ -> True
  _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = go $ txInfoWdrl info
      where
        go :: [(StakingCredential, Integer)] -> Integer
        go [] = traceError "withdrawal not found"
        go ((cred', amt) : xs)
          | cred' == cred = amt
          | otherwise = go xs

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
          | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken
          | otherwise = n

{- HLINT ignore stakeValidator "Avoid lambda" -}
stakeValidator :: Address -> TypedStake ()
stakeValidator addr =
  mkTypedStake $
    $$(PlutusTx.compile [||\param -> toBuiltinStake (mkStakingValidator param)||])
      `PlutusTx.applyCode` PlutusTx.liftCode addr
