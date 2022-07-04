-- | Fake coins for testing
module Plutus.Test.Model.Mint(
  FakeCoin(..),
  fakeCoin,
  fakeValue,
) where

import Prelude (undefined)
-- FIXME: import PlutusTx qualified
import PlutusTx.Prelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Contexts
import Plutus.Test.Model.Fork.Ledger.Scripts

newtype FakeCoin = FakeCoin { fakeCoin'tag :: BuiltinByteString }

fakeValue :: FakeCoin -> Integer -> Value
fakeValue tag = assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> AssetClass
fakeCoin (FakeCoin tag) = assetClass sym tok
  where
    sym = scriptCurrencySymbol $ fakeMintingPolicy tag
    tok = TokenName tag

fakeMintingPolicy :: BuiltinByteString -> MintingPolicy
fakeMintingPolicy _mintParams = undefined -- FIXME

-- FICME
-- | Can mint new coins if token name equals to fixed tag.
_fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
_fakeMintingPolicyContract tag _ ctx =
  valueOf (txInfoMint $ scriptContextTxInfo ctx) (ownCurrencySymbol ctx) tag > 0
