module Suites.Plutus.Model.Script.V2.Onchain.Lend.Script (
  Lend,
  lendScript,
  LendMint,
  lendPolicy,
) where

import Prelude (($))
import PlutusTx qualified
import Suites.Plutus.Model.Script.V2.Onchain.Lend
import Plutus.Test.Model.V2 (
  toBuiltinValidator,
  TypedValidator,
  mkTypedValidator,
  toBuiltinPolicy,
  TypedPolicy,
  mkTypedPolicy
  )

type Lend = TypedValidator LendDatum LendAct

-- | The GeroGov validator script instance
lendScript :: Lend
lendScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator lendContract ||])

type LendMint = TypedPolicy ()

lendPolicy :: LendMintParams -> LendMint
lendPolicy lendMintParams =
  mkTypedPolicy $
    $$(PlutusTx.compile [|| \param -> toBuiltinPolicy (lendPolicyContract param)||])
      `PlutusTx.applyCode` PlutusTx.liftCode lendMintParams
