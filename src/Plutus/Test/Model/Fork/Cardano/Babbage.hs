module Plutus.Test.Model.Fork.Cardano.Babbage(
  Era,
  toBabbageTx,
) where

import Prelude

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParams)
import Cardano.Ledger.Babbage.Tx qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Babbage.PParams qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe(..),
  )
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
      let collateralReturn = undefined
          totalCollateral = undefined
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

    getOutputs = undefined
    getWits = undefined

