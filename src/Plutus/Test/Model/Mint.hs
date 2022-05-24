-- | Fake coins for testing
module Plutus.Test.Model.Mint (
  FakeCoin (..),
  fakeCoin,
  fakeValue,
) where

import Ledger
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import PlutusTx qualified
import PlutusTx.Prelude

newtype FakeCoin = FakeCoin {fakeCoin'tag :: BuiltinByteString}

fakeValue :: FakeCoin -> Integer -> Value
fakeValue tag = assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> AssetClass
fakeCoin (FakeCoin tag) = assetClass sym tok
  where
    sym = scriptCurrencySymbol $ fakeMintingPolicy tag
    tok = TokenName tag

fakeMintingPolicy :: BuiltinByteString -> Ledger.MintingPolicy
fakeMintingPolicy mintParams =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . fakeMintingPolicyContract||])
      `PlutusTx.applyCode` PlutusTx.liftCode (TokenName mintParams)

-- | Can mint new coins if token name equals to fixed tag.
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  valueOf (txInfoMint $ scriptContextTxInfo ctx) (ownCurrencySymbol ctx) tag > 0
