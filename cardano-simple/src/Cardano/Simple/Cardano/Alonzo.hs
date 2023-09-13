{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Alonzo era conversions
module Cardano.Simple.Cardano.Alonzo (
  Era,
  toAlonzoTx,
  toTxOut,
) where

import Prelude

import Control.Lens ((^.))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Tx qualified as C
import Cardano.Ledger.Alonzo.TxBody qualified as C
import Cardano.Ledger.Alonzo.TxWits qualified as C
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Address qualified as C
import Cardano.Ledger.Compactible qualified as C
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Mary.Value as C
import Cardano.Ledger.SafeHash
import Cardano.Ledger.SafeHash qualified as C (hashAnnotated)
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe (..),
 )
import Cardano.Simple.Cardano.Class
import Cardano.Simple.Cardano.Common (
  getDCerts,
  getFee,
  getInputsBy,
  getInterval,
  getMint,
  getSignatories,
  getWdrl,
  toAddr,
  toDataHash,
  toDatumWitness,
  toKeyWitness,
  toRedeemerWitness,
  toScriptWitness,
  toValue,
 )
import Cardano.Simple.Ledger.Tx qualified as Plutus
import Cardano.Simple.TxExtra qualified as P
import Data.Sequence.Strict qualified as Seq
import PlutusLedgerApi.V2 qualified as P
import PlutusLedgerApi.V2.Tx qualified as Plutus

type Era = AlonzoEra StandardCrypto
type ToCardanoError = String

instance IsCardanoTx Era where
  getTxBody = C.body

  toCardanoTx = toAlonzoTx

  toTxOut _ network (P.TxOut addr value mdh _) = do
    caddr <- toAddr network addr
    cvalue <- toValue value
    fullValue caddr cvalue
    where
      {- TODO: implement compact case
        case cvalue of
          C.Value ada [] ->
            case C.toCompact (Coin ada) of
              Just compactAda ->
                case caddr of
                  C.Addr network cred C.StakeRefNull ->
                    let addr28 = snd $ C.encodeAddress28 netw cred
                    in  adaOnly addr28 compactAda
                  _ -> fullValue caddr cvalue
              Nothing         -> fullValue caddr cvalue
          _              -> fullValue caddr cvalue
      -}

      {-
      adaOnly (C.Addr netw pred cred) ada = do
        let addr28 = snd $ C.encodeAddress28 netw cred
        case mdh of
          Nothing -> pure $ C.TxOut_AddrHash28_AdaOnly cred addr28 ada
          Just dh -> do
            mdh32 <- C.encodeDataHash32 <$> toDataHash dh
            case mdh32 of
                Nothing   -> Left "failed to encode data hash 32"
                Just dh32 -> pure $ C.TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28 ada dh32
          -}

      fullValue caddr cvalue = do
        cval <- toVal cvalue
        case mdh of
          Plutus.OutputDatumHash dh -> do
            cdh <- toDataHash dh
            pure $ C.TxOutCompactDH' compAddr cval cdh
          Plutus.NoOutputDatum -> pure $ C.TxOutCompact' compAddr cval
          Plutus.OutputDatum _ -> Left "Output datum not supported in alonzo era"
        where
          compAddr = C.compactAddr caddr

          toVal v =
            case C.toCompact v of
              Just cval -> Right cval
              Nothing -> Left "Fail to create compact value"

toAlonzoTx :: Network -> C.PParams Era -> P.Extra -> Plutus.Tx -> Either ToCardanoError (C.AlonzoTx Era)
toAlonzoTx network params extra tx = do
  body <- toBody
  wits <- toWits (C.hashAnnotated body) extra tx
  let isValid = C.IsValid True -- TODO or maybe False
      auxData = C.SNothing
  pure $ C.AlonzoTx body wits isValid auxData
  where
    toBody = do
      inputs <- getInputsBy Plutus.txInputs tx
      collateral <- getInputsBy Plutus.txCollateral tx
      outputs <- getOutputs tx
      txcerts <-
        getDCerts
          network
          (params ^. C.ppPoolDepositL)
          (params ^. C.ppMinPoolCostL)
          extra
      txwdrls <- getWdrl network extra
      let txfee = getFee tx
          txvldt = getInterval tx
          txUpdates = C.SNothing
          reqSignerHashes = getSignatories tx
      (C.MaryValue _ mint) <- getMint tx
      let scriptIntegrityHash = C.SNothing
          adHash = C.SNothing
          txnetworkid = C.SJust network
      pure $
        C.AlonzoTxBody
          inputs
          collateral
          outputs
          txcerts
          txwdrls
          txfee
          txvldt
          txUpdates
          reqSignerHashes
          mint
          scriptIntegrityHash
          adHash
          txnetworkid

    getOutputs =
      fmap Seq.fromList
        . mapM (toTxOut mempty network)
        . Plutus.txOutputs

toWits ::
  SafeHash StandardCrypto C.EraIndependentTxBody ->
  P.Extra ->
  Plutus.Tx ->
  Either ToCardanoError (C.AlonzoTxWits Era)
toWits txBodyHash extra tx = do
  let bootstrapWits = mempty
  datumWits <- toDatumWitness tx
  let redeemerWits = toRedeemerWitness extra tx
  scriptWits <- toScriptWitness extra tx
  pure $
    C.AlonzoTxWits
      (toKeyWitness txBodyHash tx)
      bootstrapWits
      scriptWits
      datumWits
      redeemerWits
