{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Babbage era conversions
module Cardano.Simple.Cardano.Babbage (
  Era,
  toBabbageTx,
) where

import Control.Lens ((^.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq
import Prelude

import Cardano.Ledger.Alonzo.Scripts.Data qualified as C
import Cardano.Ledger.Alonzo.TxWits qualified as C
import Cardano.Ledger.Babbage (BabbageEra, Babbage)
import Cardano.Ledger.Babbage.Tx qualified as C
import Cardano.Ledger.Babbage.TxBody qualified as C
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Address qualified as C
import Cardano.Ledger.Compactible qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Binary qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe (..),
 )
import Cardano.Simple.Cardano.Class
import Cardano.Simple.Cardano.Common (
  ToCardanoError,
  getDCerts,
  getFee,
  getInputsBy,
  getInterval,
  getMint,
  getSignatories,
  getWdrl,
  toAddr,
  toCoin,
  toDataHash,
  toDatumWitness,
  toKeyWitness,
  toRedeemerWitness,
  toScriptWitness,
  toStrictMaybe,
  toValue,
 )
import Cardano.Simple.Ledger.Scripts qualified as C (Versioned (..), toScript)
import Cardano.Simple.Ledger.Tx qualified as Plutus
import Cardano.Simple.PlutusLedgerApi.V1.Scripts qualified as P
import Cardano.Simple.TxExtra qualified as P
import PlutusLedgerApi.V2 qualified as P
import qualified Cardano.Ledger.Mary.Value as C

type Era = BabbageEra StandardCrypto

instance IsCardanoTx Era where
  getTxBody = C.body
  toCardanoTx = toBabbageTx
  toTxOut = toBabbageTxOut

toBabbageTx ::
  Network ->
  C.PParams Era ->
  P.Extra ->
  Plutus.Tx ->
  Either ToCardanoError (C.AlonzoTx Era)
toBabbageTx network params extra tx = do
  body <- getBody
  wits <- toWits (hashAnnotated body) extra tx
  let isValid = C.IsValid True -- TODO or maybe False
      auxData = C.SNothing
  pure $ C.AlonzoTx body wits isValid auxData
  where
    scriptMap = Plutus.txScripts tx
    getBody = do
      spendInputs <- getInputsBy Plutus.txInputs tx
      collateralInputs <- getInputsBy Plutus.txCollateral tx
      referenceInputs <- getInputsBy Plutus.txReferenceInputs tx
      collateralReturn <- getCollateralReturn tx
      let totalCollateral = getTotalCollateral tx
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
          txNetworkId = C.SJust network
      pure $
        C.BabbageTxBody
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
        . mapM (toSizedTxOut scriptMap network)
        . Plutus.txOutputs

    getTotalCollateral =
      maybe C.SNothing (C.SJust . toCoin)
        . Plutus.txTotalCollateral

    getCollateralReturn =
      fmap toStrictMaybe
        . mapM (toSizedTxOut scriptMap network)
        . Plutus.txCollateralReturn

toSizedTxOut ::
  Map P.ScriptHash (C.Versioned P.Script) ->
  Network ->
  P.TxOut ->
  Either ToCardanoError (C.Sized (C.BabbageTxOut Era))
toSizedTxOut scriptMap network tout = C.mkSized (C.eraProtVerHigh @Babbage) <$> toTxOut scriptMap network tout

toBabbageTxOut :: Map P.ScriptHash (C.Versioned P.Script) -> Network -> P.TxOut -> Either ToCardanoError (C.BabbageTxOut Era)
toBabbageTxOut scriptMap network (P.TxOut addr value mdh mScriptHash) = do
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
      case mScriptHash of
        Just scriptHash ->
          case Map.lookup scriptHash scriptMap of
            Just script -> fullScript compAddr cval mdh script
            Nothing -> Left "No script in the map for reference script"
        Nothing -> fullDatum compAddr cval
      where
        compAddr = C.compactAddr caddr

        toVal v =
          case C.toCompact v of
            Just cval -> Right cval
            Nothing -> Left "Fail to create compact value"

    fullScript caddr cval dat (C.Versioned lang script) = do
      cdat <- toOutputDatum dat
      pure $ C.TxOutCompactRefScript caddr cval cdat (C.toScript $ C.Versioned lang script)

    fullDatum caddr cval =
      case mdh of
        P.OutputDatumHash dh -> do
          cdh <- toDataHash dh
          pure $ C.TxOutCompactDH caddr cval cdh
        P.NoOutputDatum -> pure $ C.TxOutCompact caddr cval
        P.OutputDatum dat -> pure $ C.TxOutCompactDatum caddr cval (C.dataToBinaryData $ toDatum dat)

toOutputDatum :: P.OutputDatum -> Either ToCardanoError (C.Datum Era)
toOutputDatum = \case
  P.NoOutputDatum -> pure C.NoDatum
  P.OutputDatumHash dh -> C.DatumHash <$> toDataHash dh
  P.OutputDatum dat -> pure $ C.Datum $ C.dataToBinaryData $ toDatum dat

toDatum :: P.Datum -> C.Data Era
toDatum (P.Datum (P.BuiltinData d)) = C.Data d

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
