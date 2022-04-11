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

import Data.Proxy
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Coerce (coerce)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
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
    -> Tx
    -> Either ToCardanoError (C.TxBody C.AlonzoEra)
toCardanoTxBody sigs protocolParams networkId (Tx extra P.Tx{..}) = do
    txIns <- traverse toCardanoTxInBuild $ Set.toList txInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    txOuts <- traverse (toCardanoTxOut networkId txData) txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue txRedeemers txMint txMintScripts
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInAlonzoEra <$> traverse toCardanoPaymentKeyHash sigs
    withdrawals <- toWithdrawals (extra'withdraws extra)
    certificates <- toCertificates (extra'certificates extra)
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
        , txCertificates = certificates
        , txUpdateProposal = C.TxUpdateProposalNone
        }
    where
      toWithdrawals = \case
        [] -> pure C.TxWithdrawalsNone
        xs -> C.TxWithdrawals C.WithdrawalsInAlonzoEra <$> mapM toWithdraw xs

      toCertificates certs = case certs of
        [] -> pure C.TxCertificatesNone
        _  -> C.TxCertificates C.CertificatesInAlonzoEra <$> mapM (toCardanoCertificate networkId) dcerts <*> witnessMap
        where
          dcerts = certificate'dcert <$> certs
          witnessMap = fmap (C.BuildTxWith . Map.fromList) $ mapM toWitness $ Map.toList $ getCertificateValidators certs

          toWitness (stakeCred, (redeemer, validator)) = do
            cred <- toCardanoStakeCredential stakeCred
            val  <- toCardanoStakeWitness redeemer validator
            pure (cred, C.ScriptWitness C.ScriptWitnessForStakeAddr val)

      toWithdraw Withdraw{..} = do
        saddr <- toCardanoStakeAddress networkId withdraw'credential
        witness <- toStakeWitness
        pure (saddr, amt, witness)
        where
          amt     = C.Lovelace withdraw'amount

          toStakeWitness = case withdraw'credential of
            P.StakingHash cred -> case cred of
                P.PubKeyCredential _pkh -> pure $ C.BuildTxWith $ C.KeyWitness C.KeyWitnessForStakeAddr
                P.ScriptCredential _vh -> case withdraw'script of
                  Just (redeemer, validator) -> (C.BuildTxWith . C.ScriptWitness C.ScriptWitnessForStakeAddr) <$> toCardanoStakeWitness redeemer validator
                  Nothing                    -> Left $ TxBodyError "No script for stake validator"
            P.StakingPtr _ _ _ -> Left $ TxBodyError "StakingPtr where StakingHash expected"


toCardanoStakeAddress :: C.NetworkId -> P.StakingCredential -> Either ToCardanoError C.StakeAddress
toCardanoStakeAddress networkId credential =
  C.StakeAddress (C.toShelleyNetwork networkId) <$> (C.toShelleyStakeCredential <$> toCardanoStakeCredential credential)

toCardanoStakeCredential :: P.StakingCredential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential = \case
  P.StakingHash (P.PubKeyCredential pubKeyHash)    -> C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
  P.StakingHash (P.ScriptCredential validatorHash) -> C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash
  _                                                -> txError "StakingPtr where StakingHash expected"

toCardanoCertificate :: C.NetworkId -> P.DCert -> Either ToCardanoError C.Certificate
toCardanoCertificate networkId = \case
  P.DCertDelegRegKey stakeCred           -> C.StakeAddressRegistrationCertificate <$> toCardanoStakeCredential stakeCred
  P.DCertDelegDeRegKey stakeCred         -> C.StakeAddressDeregistrationCertificate <$> toCardanoStakeCredential stakeCred
  P.DCertDelegDelegate stakeCred poolKey -> C.StakeAddressDelegationCertificate <$> toCardanoStakeCredential stakeCred <*> toCardanoPoolId poolKey
  P.DCertPoolRegister poolKey poolVrf    -> C.StakePoolRegistrationCertificate <$> toStakePoolParameters poolKey poolVrf
  P.DCertPoolRetire poolKey epoch        -> C.StakePoolRetirementCertificate <$> toCardanoPoolId poolKey <*> (pure $ C.EpochNo $ fromIntegral epoch)
  P.DCertGenesis                         -> txError "No conversion for DCertGenesis"
  P.DCertMir                             -> txError "No conversion for DCertMir"
  where
    toStakePoolParameters poolKey poolVrf = do
      poolId    <- toCardanoPoolId poolKey
      vfrKey    <- toCardanoVrfKey poolVrf
      stakeAddr <- toCardanoStakeAddress networkId (P.StakingHash $ P.PubKeyCredential poolKey)
      pure C.StakePoolParameters
        { stakePoolId            = poolId
        , stakePoolVRF           = vfrKey
        , stakePoolCost          = 0
        , stakePoolMargin        = 0
        , stakePoolRewardAccount = stakeAddr
        , stakePoolPledge        = 0
        , stakePoolOwners        = []
        , stakePoolRelays        = []
        , stakePoolMetadata      = Nothing
        }

txError :: String -> Either ToCardanoError a
txError = Left . TxBodyError

toCardanoPoolId :: P.PubKeyHash -> Either ToCardanoError C.PoolId
toCardanoPoolId pkh = fmap coerce <$> toCardanoPaymentKeyHash $ P.PaymentPubKeyHash pkh

toCardanoVrfKey :: P.PubKeyHash -> Either ToCardanoError (C.Hash C.VrfKey)
toCardanoVrfKey pkh = castHash "VrfKey" =<< (toCardanoPaymentKeyHash $ P.PaymentPubKeyHash pkh)

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

castHash :: forall a b. (C.SerialiseAsRawBytes (C.Hash a), C.SerialiseAsRawBytes (C.Hash b))
  => String -> C.Hash a -> Either ToCardanoError (C.Hash b)
castHash msg =
  maybe err Right . C.deserialiseFromRawBytes (C.proxyToAsType (Proxy :: Proxy (C.Hash b))) . C.serialiseToRawBytes
  where
    err = txError $ "Failed to convert to " <> msg


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

