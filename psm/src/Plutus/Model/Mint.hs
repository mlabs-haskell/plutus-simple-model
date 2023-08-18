-- | Fake coins for testing
module Plutus.Model.Mint (
  FakeCoin (..),
  fakeCoin,
  fakeValue,
) where

import Prelude (either, error, id, ($), (.))

import Data.Text qualified as Text

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api.S
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V2.Contexts
import PlutusLedgerApi.V1.Value
import qualified PlutusTx

newtype FakeCoin = FakeCoin {fakeCoin'tag :: BuiltinByteString}

fakeValue :: FakeCoin -> PlutusTx.Integer -> Value
fakeValue tag = assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> AssetClass
fakeCoin (FakeCoin tag) = assetClass sym tok
  where
    sym =
      CurrencySymbol $
        Api.serialiseToRawBytes $ Api.hashScript $ Api.PlutusScript Api.PlutusScriptV2 script
          $ Api.S.PlutusScriptSerialised $ serialiseCompiledCode $ fakeMintingPolicy tag
    tok = TokenName tag

fakeMintingPolicy :: BuiltinByteString -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
fakeMintingPolicy mintParam =
  $$(PlutusTx.compile [|| fakeMintingPolicyUntypedContract ||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode (TokenName mintParam)

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINEABLE fakeMintingPolicyContract #-}
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag > 0

-- | See `fakeMintingPolicyContract`.
{-# INLINEABLE fakeMintingPolicyUntypedContract #-}
fakeMintingPolicyUntypedContract :: TokenName -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
fakeMintingPolicyUntypedContract tag red ctx = PlutusTx.check (fakeMintingPolicyContract tag (PlutusTx.unsafeFromBuiltinData red) (PlutusTx.unsafeFromBuiltinData ctx))
