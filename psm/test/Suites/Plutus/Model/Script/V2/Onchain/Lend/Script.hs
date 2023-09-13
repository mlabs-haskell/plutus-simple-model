-- | Compiled script for Lend example
module Suites.Plutus.Model.Script.V2.Onchain.Lend.Script (
  Lend,
  lendScript,
  LendMint,
  lendPolicy,
) where

import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  mkTypedValidator,
  toBuiltinPolicy,
  toBuiltinValidator,
 )

import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Lend

import Prelude (($))
import PlutusCore.Version (plcVersion100)

type Lend = TypedValidator LendDatum LendAct

-- | The TypedValidator for Lend contract
lendScript :: Lend
lendScript = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator lendContract||])

type LendMint = TypedPolicy ()

{- HLINT ignore lendPolicy "Avoid lambda" -}
lendPolicy :: LendMintParams -> LendMint
lendPolicy lendMintParams =
  mkTypedPolicy $
    $$(PlutusTx.compile [||\param -> toBuiltinPolicy (lendPolicyContract param)||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 lendMintParams
