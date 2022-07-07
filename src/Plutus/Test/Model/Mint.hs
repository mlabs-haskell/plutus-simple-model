-- | Fake coins for testing
module Plutus.Test.Model.Mint(
  FakeCoin(..),
  fakeCoin,
  fakeValue,
) where

import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx qualified
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
fakeMintingPolicy mintParams =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||
      \params redeemer ctx ->
        PlutusTx.check (fakeMintingPolicyContract params (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx))
       ||])
      `PlutusTx.applyCode` PlutusTx.liftCode (TokenName mintParams)

-- | Can mint new coins if token name equals to fixed tag.
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag > 0

