{- | Nft contract. User can create NFT that is safe to assume unique
 by depenedence on concrete @TxOutRef@.
-}
module Suites.Plutus.Model.Script.V1.Onchain.Nft (
  NftParams (NftParams),
  nftContract,
  nftMintingPolicy,
  nftCurrencySymbol,
  nftValue,
) where

import Prelude

import Plutus.Model.V1
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Contexts (ownCurrencySymbol)

import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

data NftParams = NftParams TxOutRef TokenName

{-# INLINEABLE nftContract #-}
nftContract :: NftParams -> () -> ScriptContext -> Bool
nftContract (NftParams ref tok) _ ctx =
  {- check that ref is in the inputs
     and that we minted given token with value that equals to 1
  -}
  hasUtxo && checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUtxo :: Bool
    hasUtxo = any (\inp -> txInInfoOutRef inp Plutus.== ref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount =
      txInfoMint info Plutus.== singleton (ownCurrencySymbol ctx) tok 1

----------------------------------------------------------
-- instances

PlutusTx.unstableMakeIsData ''NftParams
PlutusTx.makeLift ''NftParams

----------------------------------------------------------
-- compiled code

{- HLINT ignore nftMintingPolicy "Avoid lambda" -}
nftMintingPolicy :: NftParams -> TypedPolicy ()
nftMintingPolicy nftp =
  mkTypedPolicy $
    $$(PlutusTx.compile [||\param -> toBuiltinPolicy (nftContract param)||])
      `PlutusTx.applyCode` PlutusTx.liftCode nftp

nftCurrencySymbol :: NftParams -> CurrencySymbol
nftCurrencySymbol = scriptCurrencySymbol . nftMintingPolicy

nftValue :: NftParams -> Value
nftValue nftp@(NftParams _ tok) = singleton (nftCurrencySymbol nftp) tok 1
