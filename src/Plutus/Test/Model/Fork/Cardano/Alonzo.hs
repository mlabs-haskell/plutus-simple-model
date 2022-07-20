module Plutus.Test.Model.Fork.Cardano.Alonzo(
  Era,
  toAlonzoTx,
  fromTxId,
  toAddr,
  toTxOut,
  toTxIn,
  toUtxo,
) where

import Prelude

import Data.Map qualified as Map
import Data.Sequence.Strict qualified as Seq
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.TxIn qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Alonzo (AlonzoEra, PParams)
import Cardano.Ledger.Alonzo.Tx qualified as C
import Cardano.Ledger.Alonzo.TxBody qualified as C
import Cardano.Ledger.Compactible qualified as C
import Cardano.Ledger.CompactAddress qualified as C
import Cardano.Ledger.SafeHash qualified as C (hashAnnotated)
import Cardano.Ledger.Shelley.UTxO qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe(..),
  )
import Cardano.Ledger.Alonzo.PParams qualified as C
import Cardano.Ledger.Alonzo.TxWitness qualified as C
import Plutus.Test.Model.Fork.TxExtra qualified as P
import Plutus.V2.Ledger.Api qualified as P
import Plutus.V2.Ledger.Tx qualified as Plutus
import PlutusTx.Builtins.Internal qualified as P
import Plutus.Test.Model.Fork.Ledger.Tx qualified as Plutus
import Plutus.Test.Model.Fork.Cardano.Common(
  getInputsBy,
  getFee,
  getInterval,
  getMint,
  getDCerts,
  getSignatories,
  getWdrl,
  toValue,
  toTxIn,
  toAddr,
  toDataHash,
  toDatumWitness,
  toKeyWitness,
  toRedeemerWitness,
  toScriptWitness,
  )
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Hashes qualified as C
import Cardano.Crypto.Hash.Class
import Data.ByteString.Short (fromShort)

type Era = AlonzoEra StandardCrypto
type ToCardanoError = String

toAlonzoTx :: Network -> PParams Era -> P.Tx -> Either ToCardanoError (C.ValidatedTx Era)
toAlonzoTx network params tx = do
  body <- toBody
  wits <- toWits (C.hashAnnotated body) tx
  let isValid = C.IsValid True -- TODO or maybe False
      auxData = C.SNothing
  pure $ C.ValidatedTx body wits isValid auxData
  where
    toBody = do
      inputs <- getInputsBy Plutus.txInputs tx
      collateral <- getInputsBy Plutus.txCollateral tx
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
          txnetworkid = C.SJust network
      pure $
        C.TxBody
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
      . mapM (toTxOut network)
      . Plutus.txOutputs
      . P.tx'plutus

toWits :: SafeHash StandardCrypto C.EraIndependentTxBody -> P.Tx -> Either ToCardanoError (C.TxWitness Era)
toWits txBodyHash tx = do
  let bootstrapWits = mempty
  datumWits <- toDatumWitness tx
  let redeemerWits = toRedeemerWitness tx
  scriptWits <- toScriptWitness tx
  pure $ C.TxWitness (toKeyWitness txBodyHash tx) bootstrapWits scriptWits datumWits redeemerWits

fromTxId :: C.TxId StandardCrypto -> P.TxId
fromTxId (C.TxId safeHash) =
  case extractHash safeHash of
    UnsafeHash shortBs -> P.TxId $ P.BuiltinByteString $ fromShort shortBs

-- toTxIn :: P.TxOutRef -> Either ToCardanoError (C.TxIn StandardCrypto)

toUtxo :: Network -> [(P.TxOutRef, P.TxOut)] -> Either ToCardanoError (C.UTxO Era)
toUtxo network xs = C.UTxO . Map.fromList <$> mapM go xs
  where
    go (tin, tout) = do
      tinC <- toTxIn tin
      toutC <- toTxOut network tout
      pure (tinC, toutC)

toTxOut :: Network -> P.TxOut -> Either ToCardanoError (C.TxOut Era)
toTxOut network (P.TxOut addr value mdh _) = do
  caddr <- toAddr network addr
  cvalue <- toValue value
  fullValue caddr cvalue
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
  where
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
            Nothing   -> Left "Fail to create compact value"

