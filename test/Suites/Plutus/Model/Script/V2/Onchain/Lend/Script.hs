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
  -- mkTypedPolicy,
  -- mkTypedValidator,
  -- toBuiltinPolicy,
  -- toBuiltinValidator,
 )

-- import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Lend

-- import Prelude (($))
import PlutusTx.Builtins (error)

type Lend = TypedValidator LendDatum LendAct

-- | The TypedValidator for Lend contract
lendScript :: Lend
lendScript = error ()

-- lendScript = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator lendContract||])

type LendMint = TypedPolicy ()

lendPolicy :: LendMintParams -> LendMint
lendPolicy _lendMintParams = error ()

-- mkTypedPolicy $
--   $$(PlutusTx.compile [||\param -> toBuiltinPolicy (lendPolicyContract param)||])
--     `PlutusTx.applyCode` PlutusTx.liftCode lendMintParams
