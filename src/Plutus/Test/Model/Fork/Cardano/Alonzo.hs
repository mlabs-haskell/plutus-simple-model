module Plutus.Test.Model.Fork.Cardano.Alonzo(
  Era,
  toAlonzoTx,
  fromTxId,
  toAddr,
  toValue,
  toTxOut,
  toTxIn,
  toUtxo,
) where

import Prelude
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Bifunctor
import Data.Default (def)
import Cardano.Crypto.Hash.Class qualified as C
import Data.ByteString qualified as BS
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.TxIn qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Coin qualified as C
import Cardano.Ledger.Alonzo (AlonzoEra, PParams)
import Cardano.Ledger.Alonzo.Tx qualified as C
import Cardano.Ledger.Alonzo.TxBody qualified as C
import Cardano.Ledger.Credential qualified as C
import Cardano.Ledger.Hashes qualified as C
import Cardano.Ledger.Keys qualified as C
import Cardano.Ledger.Address qualified as C
import Cardano.Ledger.Mary.Value qualified as C
import Cardano.Ledger.Slot qualified as C
import Cardano.Ledger.Compactible qualified as C
import Cardano.Ledger.CompactAddress qualified as C
import Cardano.Ledger.Shelley.UTxO qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as Shelley (Hash)
import Cardano.Ledger.Shelley.API.Types qualified as C (StrictMaybe(..))
import Cardano.Ledger.Shelley.TxBody qualified as C (DCert(..), Wdrl(..))
import Cardano.Ledger.ShelleyMA.Timelocks qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C (PoolParams(..), PoolCert(..), DelegCert(..), Delegation(..))
import Cardano.Ledger.Alonzo.PParams qualified as C
import qualified Cardano.Crypto.Hash.Class as Crypto
import Plutus.Test.Model.Fork.TxExtra qualified as P
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Tx qualified as P
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Builtins.Internal qualified as P
import PlutusTx.Builtins qualified as PlutusTx
import Plutus.Test.Model.Fork.Ledger.Tx qualified as Plutus
import  Plutus.Test.Model.Fork.Ledger.Crypto qualified as P
import  Plutus.Test.Model.Fork.Ledger.Slot qualified as P

import Cardano.Ledger.SafeHash
import Cardano.Crypto.Hash.Class
import Data.ByteString.Short (fromShort, toShort)

type Era = AlonzoEra StandardCrypto
type ToCardanoError = String

toAlonzoTx :: Network -> PParams Era -> P.Tx -> Either ToCardanoError (C.ValidatedTx Era)
toAlonzoTx network params tx = do
  body <- toBody
  wits <- toWits
  let isValid = C.IsValid True -- TODO or maybe False
      auxData = C.SNothing
  pure $ C.ValidatedTx body wits isValid auxData
  where
    toBody = do
      inputs <- getInputsBy Plutus.txInputs tx
      collateral <- getInputsBy Plutus.txCollateral tx
      outputs <- getOutputs tx
      txcerts <- getDCerts tx
      txwdrls <- getWdrl tx
      let txfee = getFee tx
          txvldt = getInterval tx
          txUpdates = C.SNothing
      reqSignerHashes <- getSignatories tx
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

    getInputsBy extract =
        fmap Set.fromList
      . mapM toTxIn
      . fmap P.txInRef
      . Set.toList
      . extract
      . P.tx'plutus

    getOutputs =
        fmap Seq.fromList
      . mapM (toTxOut network)
      . Plutus.txOutputs
      . P.tx'plutus

    getFee = C.Coin . getLovelace . Plutus.txFee . P.tx'plutus

    getInterval = toInterval . Plutus.txValidRange . P.tx'plutus

    getSignatories =
        fmap Set.fromList
      . mapM (toPubKeyHash . P.pubKeyHash)
      . Map.keys
      . Plutus.txSignatures
      . P.tx'plutus

    getMint = toValue . Plutus.txMint . P.tx'plutus

    getWdrl =
        toWdrl network
      . P.extra'withdraws
      . P.tx'extra

    getDCerts =
        fmap Seq.fromList
      . mapM (toDCert network params . P.certificate'dcert)
      . P.extra'certificates
      . P.tx'extra

    toWits = undefined


    getLovelace v = Value.valueOf v Value.adaSymbol Value.adaToken

-- | TODO: interpret closures
toInterval :: P.SlotRange -> C.ValidityInterval
toInterval (P.Interval (P.LowerBound from _) (P.UpperBound to _)) = C.ValidityInterval before after
  where
    before = case from of
      P.Finite a -> C.SJust (toSlot a)
      _        -> C.SNothing

    after = case to of
      P.Finite a -> C.SJust  (toSlot a)
      _        -> C.SNothing

toSlot :: P.Slot -> C.SlotNo
toSlot (P.Slot n) = C.SlotNo (fromInteger n)

toWdrl :: Network -> [P.Withdraw] -> Either ToCardanoError (C.Wdrl StandardCrypto)
toWdrl network ws = C.Wdrl . Map.fromList <$> mapM to ws
  where
    to (P.Withdraw scred amount _) =
      case scred of
        P.StakingHash cred -> (\x -> (C.RewardAcnt network x, C.Coin amount)) <$> toCredential cred
        _                  -> Left "Not supported staking cred in withdraw"

toDCert :: Network -> PParams Era -> P.DCert -> Either ToCardanoError (C.DCert StandardCrypto)
toDCert network params = \case
  P.DCertDelegRegKey (P.StakingHash stakingCredential) -> C.DCertDeleg . C.RegKey <$> toCredential stakingCredential
  P.DCertDelegDeRegKey (P.StakingHash stakingCredential) -> C.DCertDeleg . C.DeRegKey <$> toCredential stakingCredential
  P.DCertDelegDelegate (P.StakingHash stakingCredential) pubKeyHash -> C.DCertDeleg . C.Delegate <$> (C.Delegation <$> toCredential stakingCredential <*> toPubKeyHash pubKeyHash)
  P.DCertPoolRegister poolKeyHash poolVfr -> C.DCertPool . C.RegPool <$> toPoolParams poolKeyHash poolVfr
  P.DCertPoolRetire pkh n -> C.DCertPool . (\key -> C.RetirePool key (C.EpochNo (fromIntegral n)) ) <$> toPubKeyHash pkh
  P.DCertGenesis -> Left "DCertGenesis is not supported"
  P.DCertMir -> Left "DCertMir is not supported"
  other -> Left ("not supported DCert: " <> show other)
  where
    toPoolParams pkh vfr = do
      cpkh <- toPubKeyHash pkh
      cpkh2 <- toPubKeyHash pkh
      cpkh3 <- toPubKeyHash pkh
      cvfr <- toVrf vfr
      pure $ C.PoolParams
                cpkh
                (C.castHash cvfr)
                (C._poolDeposit params)
                (C._minPoolCost params)
                def
                (C.RewardAcnt network (C.KeyHashObj cpkh3))
                (Set.singleton $ cpkh2)
                Seq.empty
                C.SNothing


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
toTxOut network (P.TxOut addr value mdh) = do
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
        Just dh -> do
          cdh <- toDataHash dh
          pure $ C.TxOutCompactDH' compAddr cval cdh
        Nothing -> pure $ C.TxOutCompact' compAddr cval
      where
        compAddr = C.compactAddr caddr

        toVal v =
          case C.toCompact v of
            Just cval -> pure $ cval
            Nothing   -> Left "Fail to create compact value"


toDataHash :: P.DatumHash -> Either ToCardanoError (C.DataHash StandardCrypto)
toDataHash (P.DatumHash bs) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toDatumHash (" <> show (BS.length bsx) <> " bytes)"
  in tag tg $ maybe (Left "Failed to get TxId Hash") Right $ unsafeMakeSafeHash <$> Crypto.hashFromBytes bsx

toTxIn :: P.TxOutRef -> Either ToCardanoError (C.TxIn StandardCrypto)
toTxIn (P.TxOutRef txId n) = (\tid -> C.TxIn tid (toEnum $ fromInteger n)) <$> toTxId txId

toTxId :: P.TxId -> Either ToCardanoError (C.TxId StandardCrypto)
toTxId (P.TxId bs) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toTxIdHash (" <> show (BS.length bsx) <> " bytes)"
  in tag tg $ maybe (Left "Failed to get TxId Hash") Right $ C.TxId . unsafeMakeSafeHash <$> Crypto.hashFromBytes bsx


toAddr :: Network -> P.Address -> Either ToCardanoError (C.Addr StandardCrypto)
toAddr network (P.Address addressCredential addressStakingCredential) =
  C.Addr network <$> toCredential addressCredential <*> toStakeAddressReference addressStakingCredential

toCredential :: P.Credential -> Either ToCardanoError (C.Credential a StandardCrypto)
toCredential = \case
  P.PubKeyCredential pubKeyHash    -> C.KeyHashObj <$> toPubKeyHash pubKeyHash
  P.ScriptCredential validatorHash -> C.ScriptHashObj <$> toScriptHash validatorHash

toStakeAddressReference :: Maybe P.StakingCredential -> Either ToCardanoError (C.StakeReference StandardCrypto)
toStakeAddressReference = \case
  Nothing -> pure C.StakeRefNull
  Just (P.StakingHash stakeCred) -> C.StakeRefBase <$> toCredential stakeCred
  Just (P.StakingPtr x y z)      -> pure $ C.StakeRefPtr $ C.Ptr (C.SlotNo $ fromIntegral x) (TxIx $ fromIntegral y) (CertIx $ fromIntegral z)

toPubKeyHash :: P.PubKeyHash -> Either ToCardanoError (C.KeyHash kr StandardCrypto)
toPubKeyHash (P.PubKeyHash bs) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toPubKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ maybe (Left "Failed to get Hash") Right $ C.KeyHash <$> Crypto.hashFromBytes bsx

toVrf :: P.PubKeyHash -> Either ToCardanoError (Shelley.Hash StandardCrypto (C.VerKeyVRF StandardCrypto))
toVrf (P.PubKeyHash bs) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toVrfHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ maybe (Left "Failed to get VRF Hash") Right $ Crypto.hashFromBytes bsx

toScriptHash :: P.ValidatorHash -> Either ToCardanoError (C.ScriptHash StandardCrypto)
toScriptHash (P.ValidatorHash bs) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toScriptHash (" <> show (BS.length bsx) <> " bytes)"
  in  tag tg $ maybe (Left "Failed to get Script hash") Right $ C.ScriptHash <$> Crypto.hashFromBytes bsx

toValue :: P.Value -> Either ToCardanoError (C.Value StandardCrypto)
toValue val = C.valueFromList totalAda <$> traverse fromValue vs
  where
    (totalAda, vs) = foldForAda $ Value.flattenValue val

    foldForAda = L.foldl' go (0, [])
      where
        go (ada, rest) coin@(cs, tok, amount)
          | cs == Value.adaSymbol && tok == Value.adaToken = (ada + amount, rest)
          | otherwise = (ada, coin : rest)

    fromValue (cs, tok, amount) = (\p -> (p, toAssetName tok, amount)) <$> toPolicyId cs

toPolicyId :: P.CurrencySymbol -> Either ToCardanoError (C.PolicyID StandardCrypto)
toPolicyId (P.CurrencySymbol bs) = fmap C.PolicyID $ toScriptHash (P.ValidatorHash bs)

toAssetName :: P.TokenName -> C.AssetName
toAssetName (P.TokenName bs) = C.AssetName $ toShort $ PlutusTx.fromBuiltin bs

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (\x -> "tag " <> s <> " :" <> x)

