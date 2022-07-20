module Plutus.Test.Model.Fork.Cardano.Babbage(
  Era,
  toBabbageTx,
) where

import Prelude
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq

import Cardano.Ledger.SafeHash
import Cardano.Ledger.Hashes qualified as C
import Cardano.Ledger.Alonzo.Data qualified as C
import Cardano.Ledger.Alonzo.TxWitness qualified as C
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core qualified as C()
import Cardano.Ledger.Compactible qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.CompactAddress qualified as C
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParams)
import Cardano.Ledger.Babbage.Tx qualified as C
import Cardano.Ledger.Babbage.PParams qualified as C
import Cardano.Ledger.Serialization qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe(..),
  )
import Cardano.Ledger.Babbage.TxBody qualified as C
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
  toDataHash,
  toValue,
  toAddr,
  toDatumWitness,
  toKeyWitness,
  toRedeemerWitness,
  toScriptWitness,
  )
import Plutus.Test.Model.Fork.Ledger.Scripts qualified as C (Versioned(..), toScript)

type Era = BabbageEra StandardCrypto

toBabbageTx ::
     Map P.ScriptHash (C.Versioned P.Script)
  -> Network -> PParams Era -> P.Tx -> Either ToCardanoError (C.ValidatedTx Era)
toBabbageTx scriptMap network params tx = do
  body <- getBody
  wits <- toWits (hashAnnotated body) tx
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
      . mapM (toSizedTxOut scriptMap network)
      . Plutus.txOutputs
      . P.tx'plutus

    getTotalCollateral =
        maybe C.SNothing (C.SJust . toCoin)
      . Plutus.txTotalCollateral
      . P.tx'plutus

    getCollateralReturn =
        fmap toStrictMaybe
      . mapM (toSizedTxOut scriptMap network)
      . Plutus.txCollateralReturn
      . P.tx'plutus

toSizedTxOut ::
     Map P.ScriptHash (C.Versioned P.Script)
  -> Network -> P.TxOut -> Either ToCardanoError (C.Sized (C.TxOut Era))
toSizedTxOut scriptMap network tout = C.mkSized <$> toTxOut scriptMap network tout

toTxOut :: Map P.ScriptHash (C.Versioned P.Script) -> Network -> P.TxOut -> Either ToCardanoError (C.TxOut Era)
toTxOut scriptMap network (P.TxOut addr value mdh mScriptHash) = do
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
      case mScriptHash of
        Just scriptHash ->
          case Map.lookup scriptHash scriptMap of
            Just script -> fullScript compAddr cval mdh script
            Nothing     -> Left "No script in the map for reference script"
        Nothing        -> fullDatum compAddr cval
      where
        compAddr = C.compactAddr caddr

        toVal v =
          case C.toCompact v of
            Just cval -> Right cval
            Nothing   -> Left "Fail to create compact value"

    fullScript caddr cval dat (C.Versioned lang script) = do
      cdat <- toOutputDatum dat
      pure $ C.TxOutCompactRefScript caddr cval cdat (C.toScript $ C.Versioned lang script)

    fullDatum caddr cval =
      case mdh of
        P.OutputDatumHash dh -> do
          cdh <- toDataHash dh
          pure $ C.TxOutCompactDH caddr cval cdh
        P.NoOutputDatum -> pure $ C.TxOutCompact caddr cval
        P.OutputDatum _ -> Left "Output datum not supported in alonzo era"

toOutputDatum :: P.OutputDatum -> Either ToCardanoError (C.Datum Era)
toOutputDatum = \case
  P.NoOutputDatum      -> pure $ C.NoDatum
  P.OutputDatumHash dh -> C.DatumHash <$> toDataHash dh
  P.OutputDatum dat    -> pure $ C.Datum $ C.dataToBinaryData $ toDatum dat

toDatum :: P.Datum -> C.Data Era
toDatum (P.Datum (P.BuiltinData d)) = C.Data d

toWits :: SafeHash StandardCrypto C.EraIndependentTxBody -> P.Tx -> Either ToCardanoError (C.TxWitness Era)
toWits txBodyHash tx = do
  let bootstrapWits = mempty
  datumWits <- toDatumWitness tx
  let redeemerWits = toRedeemerWitness tx
  scriptWits <- toScriptWitness tx
  pure $ C.TxWitness (toKeyWitness txBodyHash tx) bootstrapWits scriptWits datumWits redeemerWits

