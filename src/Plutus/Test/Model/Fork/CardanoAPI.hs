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

Interface to the transaction types from 'cardano-api'

-}
module Plutus.Test.Model.Fork.CardanoAPI (
  toCardanoTxBody
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Ledger.Address qualified as P
import Plutus.V1.Ledger.Tx qualified as P

import Plutus.Test.Model.Fork.CardanoAPITemp (makeTransactionBody')
import Prelude
import Ledger.Tx.CardanoAPI hiding (toCardanoTxBody)

toCardanoTxBody
    :: [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> Maybe C.ProtocolParameters -- ^ Protocol parameters to use. Building Plutus transactions will fail if this is 'Nothing'
    -> C.NetworkId -- ^ Network ID
    -> P.Tx
    -> Either ToCardanoError (C.TxBody C.AlonzoEra)
toCardanoTxBody sigs protocolParams networkId P.Tx{..} = do
    txIns <- traverse toCardanoTxInBuild $ Set.toList txInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    txOuts <- traverse (toCardanoTxOut networkId txData) txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue txRedeemers txMint txMintScripts
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInAlonzoEra <$> traverse toCardanoPaymentKeyHash sigs
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
        , txWithdrawals = C.TxWithdrawalsNone
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

toCardanoTxInBuild :: P.TxIn -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.AlonzoEra))
toCardanoTxInBuild (P.TxIn txInRef (Just txInType)) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness txInType)
toCardanoTxInBuild (P.TxIn _ Nothing) = Left MissingTxInType





