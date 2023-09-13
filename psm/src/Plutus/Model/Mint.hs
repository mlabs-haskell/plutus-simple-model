{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Fake coins for testing
module Plutus.Model.Mint (
  FakeCoin (..),
  fakeCoin,
  fakeValue,
) where

import Prelude (($))

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api.S
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V2.Contexts
import PlutusLedgerApi.V1.Value
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Bool
import PlutusTx.Prelude qualified as PlutusTx
import PlutusCore.Version (plcVersion100)

newtype FakeCoin = FakeCoin {fakeCoin'tag :: BuiltinByteString}

fakeValue :: FakeCoin -> Integer -> Value
fakeValue tag = assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> AssetClass
fakeCoin (FakeCoin tag) = assetClass sym tok
  where
    sym =
      CurrencySymbol $ toBuiltin $
        Api.serialiseToRawBytes $ Api.hashScript $ Api.PlutusScript Api.PlutusScriptV2
          $ Api.S.PlutusScriptSerialised $ serialiseCompiledCode $ fakeMintingPolicy tok
    tok = TokenName tag

fakeMintingPolicy :: TokenName -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
fakeMintingPolicy mintParam =
  $$(PlutusTx.compile [|| fakeMintingPolicyUntypedContract ||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 mintParam

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINEABLE fakeMintingPolicyContract #-}
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag PlutusTx.> 0

-- | See `fakeMintingPolicyContract`.
{-# INLINEABLE fakeMintingPolicyUntypedContract #-}
fakeMintingPolicyUntypedContract :: TokenName -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
fakeMintingPolicyUntypedContract tag red ctx = PlutusTx.check (fakeMintingPolicyContract tag (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx))
