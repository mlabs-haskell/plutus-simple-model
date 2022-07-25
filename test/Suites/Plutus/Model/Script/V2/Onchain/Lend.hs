{- | Onchain code for guess a hash game.

 User can submit value to script that is locked by the hash of some secret.
 If other user can guess the hash then user can grab the value.
-}
module Suites.Plutus.Model.Script.V2.Onchain.Lend (
  LendDatum (..),
  LendAct (..),
  LendHash(..),
  LendMintParams(..),
  lendContract,
  lendPolicyContract,
) where

import PlutusTx.Prelude
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Util

data LendDatum = LendDatum
  { lendDatum'symbol :: CurrencySymbol
  , lendDatum'token  :: TokenName
  }

data LendAct = Exchange

{-# inlinable lendContract #-}
lendContract :: LendDatum -> LendAct -> ScriptContext -> Bool
lendContract (LendDatum sym tok) Exchange ctx =
  traceIfFalse "Value exhange preserved"
    (lovelaceValueOf outVal - lovelaceValueOf inVal == mintedAmount) &&
  traceIfFalse "Same datum"
    (txOutDatum tin == txOutDatum tout)
  where
    info = scriptContextTxInfo ctx
    (tin, tout) = getThrough ctx
    inVal = txOutValue tin
    outVal = txOutValue tout
    mintedAmount = valueOf (txInfoMint info) sym tok

newtype LendHash = LendHash ValidatorHash
newtype LendMintParams = LendMintParams LendHash

{-# inlinable lendPolicyContract #-}
lendPolicyContract :: LendMintParams -> () -> ScriptContext -> Bool
lendPolicyContract (LendMintParams(LendHash lendVh)) _ ctx =
  traceIfFalse "Lend contract is spent with Exchange redeemer"
    (forwardTo lendVh Exchange info)
  where
    info = scriptContextTxInfo ctx

PlutusTx.unstableMakeIsData ''LendDatum
PlutusTx.unstableMakeIsData ''LendAct
PlutusTx.makeLift ''LendHash
PlutusTx.makeLift ''LendMintParams
