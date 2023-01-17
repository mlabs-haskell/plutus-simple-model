-- | Fake coins for testing
module Plutus.Model.Mint (
  FakeCoin (..),
  fakeCoin,
  fakeValue,
) where

import Prelude (either, error, id, ($), (.))

import Data.Text qualified as Text

import PlutusLedgerApi.V1

-- import PlutusLedgerApi.V1.Contexts

import Plutarch
import Plutarch.Api.V1 as Plutarch
import Plutarch.Api.V1.Value
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V1.Value
import PlutusTx.Prelude qualified as PlutusTx

newtype FakeCoin = FakeCoin {fakeCoin'tag :: BuiltinByteString}

fakeValue :: FakeCoin -> PlutusTx.Integer -> Value
fakeValue tag = assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> AssetClass
fakeCoin (FakeCoin tag) = assetClass sym tok
  where
    sym =
      CurrencySymbol $
        getScriptHash $
          Plutarch.scriptHash $
            fakeMintingPolicy tag
    tok = TokenName tag

fakeMintingPolicy :: BuiltinByteString -> Plutarch.Script
fakeMintingPolicy mintParam =
  either (error . Text.unpack) id $
    compile (Config {tracingMode = DetTracing}) $
      fakeMintingPolicyScript mintParam

fakeMintingPolicyScript :: BuiltinByteString -> ClosedTerm PMintingPolicy
fakeMintingPolicyScript mintParam = plam $ \_redeemer ctx ->
  pif
    (0 #< pvalueOf # mint ctx # cs ctx # pconstant (TokenName mintParam))
    (popaque $ pcon PUnit)
    perror
  where
    deCtx ::
      Term s PScriptContext ->
      ( HRec
          '[ '("txInfo", Term s (PAsData PTxInfo))
           , '("purpose", Term s (PAsData PScriptPurpose))
           ] ->
        Term s b
      ) ->
      Term s b
    deCtx ctx cont =
      pmatch ctx $ \(PScriptContext r) ->
        pletFields @'["purpose", "txInfo"] r cont

    cs ctx =
      deCtx
        ctx
        ( \hr -> pmatch (pfromData $ getField @"purpose" hr) $ \case
            PMinting tdcs -> pfromData $ pfield @"_0" # tdcs
            _ -> perror
        )

    mint ctx =
      pmatch (deCtx ctx (pfromData . getField @"txInfo")) $ \(PTxInfo txi) ->
        pfromData $ pfield @"mint" # txi
