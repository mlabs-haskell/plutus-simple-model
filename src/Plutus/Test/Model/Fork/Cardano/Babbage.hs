module Plutus.Test.Model.Fork.Cardano.Babbage(
  Era,
  toBabbageTx,
) where

import Prelude
import Data.Sequence.Strict qualified as Seq

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParams)
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Babbage.Tx qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Babbage.PParams qualified as C
import Cardano.Ledger.Serialization qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe(..),
  )
import Plutus.V2.Ledger.Api qualified as P
import Plutus.Test.Model.Fork.TxExtra qualified as P
import Plutus.Test.Model.Fork.Ledger.Tx qualified as Plutus
import Plutus.Test.Model.Fork.Cardano.Common(
  ToCardanoError,
  getInputsBy,
  getInterval,
  getFee,
  getSignatories,
  getMint,
  getDCerts,
  getWdrl,
  toCoin,
  toStrictMaybe,
  )

type Era = BabbageEra StandardCrypto

toBabbageTx :: Network -> PParams Era -> P.Tx -> Either ToCardanoError (C.ValidatedTx Era)
toBabbageTx network params tx = do
  body <- getBody
  wits <- getWits
  let isValid = C.IsValid True -- TODO or maybe False
      auxData = C.SNothing
  pure $ C.ValidatedTx body wits isValid auxData
  where
    getBody = do
      spendInputs <- getInputsBy Plutus.txInputs tx
      collateralInputs <- getInputsBy Plutus.txCollateral tx
      referenceInputs <- getInputsBy Plutus.txReferenceInputs tx
      collateralReturn <- getCollateralReturn tx
      let totalCollateral = getTotalCollateral tx
      outputs <- getOutputs tx
      txcerts <- getDCerts network (C._poolDeposit params) (C._minPoolCost params) tx
      txwdrls <- getWdrl network tx
      let txfee = getFee tx
          txvldt = getInterval tx
          txUpdates = C.SNothing
          reqSignerHashes = getSignatories tx
      mint <- getMint tx
      let scriptIntegrityHash = C.SNothing
          adHash = C.SNothing
          txNetworkId = C.SJust network
      pure $ C.TxBody
        spendInputs
        collateralInputs
        referenceInputs
        outputs
        collateralReturn
        totalCollateral
        txcerts
        txwdrls
        txfee
        txvldt
        txUpdates
        reqSignerHashes
        mint
        scriptIntegrityHash
        adHash
        txNetworkId

    getOutputs =
        fmap Seq.fromList
      . mapM (toSizedTxOut network)
      . Plutus.txOutputs
      . P.tx'plutus

    getWits = undefined

    getTotalCollateral =
        maybe C.SNothing (C.SJust . toCoin)
      . Plutus.txTotalCollateral
      . P.tx'plutus

    getCollateralReturn =
        fmap toStrictMaybe
      . mapM (toSizedTxOut network)
      . Plutus.txCollateralReturn
      . P.tx'plutus

toSizedTxOut :: Network -> P.TxOut -> Either ToCardanoError (C.Sized (C.TxOut Era))
toSizedTxOut _network _tout = undefined


