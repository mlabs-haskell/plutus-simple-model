{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans        #-}

{-|

Fork for toCardanoTxBody to allow adding extra fields that are
not accessible from standard Plutus TX

-}
module Plutus.Test.Model.Fork.CardanoAPI (
  toCardanoTxBody,
  toCardanoStakeWitness,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Ledger.Address qualified as P
import Plutus.V1.Ledger.Tx qualified as P
import Plutus.V1.Ledger.Api qualified as Api
import Plutus.V1.Ledger.Api qualified as P
import Codec.Serialise qualified as Codec

import Plutus.Test.Model.Fork.CardanoAPITemp (makeTransactionBody')
import Prelude
import Ledger.Tx.CardanoAPI hiding (toCardanoTxBody)
import Plutus.Test.Model.Fork.TxExtra
import PlutusTx.Prelude qualified as PlutusTx

toCardanoTxBody
    :: [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> Maybe C.ProtocolParameters -- ^ Protocol parameters to use. Building Plutus transactions will fail if this is 'Nothing'
    -> C.NetworkId -- ^ Network ID
    -> TxExtra
    -> Either ToCardanoError (C.TxBody C.AlonzoEra)
toCardanoTxBody sigs protocolParams networkId (TxExtra extra P.Tx{..}) = do
    txIns <- traverse toCardanoTxInBuild $ Set.toList txInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    txOuts <- traverse (toCardanoTxOut networkId txData) txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue txRedeemers txMint txMintScripts
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInAlonzoEra <$> traverse toCardanoPaymentKeyHash sigs
    withdrawals <- toWithdrawals (extra'withdraws extra)
    first (TxBodyError . C.displayError) $ makeTransactionBody' C.TxBodyContent
        { txIns = txIns
        , txInsCollateral = txInsCollateral
        , txOuts = txOuts
        , txFee = txFee'
        , txValidityRange = txValidityRange
        , txMintValue = txMintValue
        , txProtocolParams = C.BuildTxWith protocolParams
        , txScriptValidity = C.TxScriptValidityNone
        , txExtraKeyWits
        -- unused:
        , txMetadata = C.TxMetadataNone
        , txAuxScripts = C.TxAuxScriptsNone
        , txWithdrawals = withdrawals
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }
    where
      toWithdrawals = \case
        [] -> pure C.TxWithdrawalsNone
        xs -> C.TxWithdrawals C.WithdrawalsInAlonzoEra <$> mapM toWithdraw xs

      toWithdraw Withdraw{..} = do
        saddr <- C.StakeAddress (C.toShelleyNetwork networkId) <$> (C.toShelleyStakeCredential <$> toStakeCredential withdraw'credential)
        witness <- toStakeWitness
        pure (saddr, amt, witness)
        where
          amt     = C.Lovelace withdraw'amount

          toStakeCredential = \case
            P.StakingHash cred -> toCardanoStakeCredential cred
            P.StakingPtr _ _ _ -> Left $ TxBodyError "StakingPtr where StakingHash expected"

          toStakeWitness = case withdraw'credential of
            P.StakingHash cred -> case cred of
                P.PubKeyCredential _pkh -> pure $ C.BuildTxWith $ C.KeyWitness C.KeyWitnessForStakeAddr
                P.ScriptCredential _vh -> case withdraw'script of
                  Just (redeemer, validator) -> (C.BuildTxWith . C.ScriptWitness C.ScriptWitnessForStakeAddr) <$> toCardanoStakeWitness redeemer validator
                  Nothing                    -> Left $ TxBodyError "No script for stake validator"
            P.StakingPtr _ _ _ -> Left $ TxBodyError "StakingPtr where StakingHash expected"

toCardanoStakeCredential :: P.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential (P.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (P.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoStakeKeyHash :: P.PubKeyHash -> Either ToCardanoError (C.Hash C.StakeKey)
toCardanoStakeKeyHash (P.PubKeyHash bs) = tag "toCardanoStakeKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PlutusTx.fromBuiltin bs)

toCardanoTxInBuild :: P.TxIn -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.AlonzoEra))
toCardanoTxInBuild (P.TxIn txInRef (Just txInType)) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness txInType)
toCardanoTxInBuild (P.TxIn _ Nothing) = Left MissingTxInType

toCardanoStakeWitness :: P.Redeemer -> P.StakeValidator -> Either ToCardanoError (C.ScriptWitness C.WitCtxStake C.AlonzoEra)
toCardanoStakeWitness redeemer (P.StakeValidator script) = do
    C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
        <$> toCardanoPlutusScript script
        <*> pure C.NoScriptDatumForStake
        <*> pure (C.fromPlutusData $ Api.toData redeemer)
        <*> pure zeroExecutionUnits

toCardanoPlutusScript :: P.Script -> Either ToCardanoError (C.PlutusScript C.PlutusScriptV1)
toCardanoPlutusScript =
    tag "toCardanoPlutusScript"
    . deserialiseFromRawBytes (C.AsPlutusScript C.AsPlutusScriptV1) . BSL.toStrict . Codec.serialise

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0

deserialiseFromRawBytes :: C.SerialiseAsRawBytes t => C.AsType t -> ByteString -> Either ToCardanoError t
deserialiseFromRawBytes asType = maybe (Left DeserialisationError) Right . C.deserialiseFromRawBytes asType

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (Tag s)

