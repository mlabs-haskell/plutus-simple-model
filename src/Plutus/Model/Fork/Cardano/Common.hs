module Plutus.Model.Fork.Cardano.Common(
  ToCardanoError,
  fromTxId,
  getInputsBy,
  getInterval,
  getFee,
  getMint,
  getDCerts,
  getSignatories,
  getWdrl,
  toValue,
  toTxIn,
  toAssetName,
  toPolicyId,
  toScriptHash,
  toCredential,
  toInterval,
  toSlot,
  toCoin,
  toStrictMaybe,
  toDataHash,
  toAddr,
  toStakeAddressReference,
  toKeyWitness,
  toDatumWitness,
  toRedeemerWitness,
  toScriptWitness
) where

import Prelude
import Control.Monad

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Short (toShort, fromShort)
import Data.Default (def)
import Data.List qualified as L
import Data.Maybe
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

import Cardano.Ledger.SafeHash
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe(..),
  Coin(..),
  PoolParams(..),
  RewardAcnt(..),
  Credential(..),
  Wdrl(..),
  Ptr(..),
  StakeReference(..),
  Addr(..),
  )
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.Era qualified as C
import Cardano.Ledger.Slot qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.Delegation.Certificates qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as Shelley (Hash)
import Cardano.Ledger.TxIn qualified as C
import Cardano.Ledger.ShelleyMA.Timelocks qualified as C
import Cardano.Ledger.Keys qualified as C
import Cardano.Ledger.Shelley.TxBody (Delegation (Delegation), WitVKey)
import Cardano.Ledger.Shelley.UTxO qualified as C
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Ledger.Mary.Value qualified as C
import Cardano.Ledger.Hashes qualified as C
import Cardano.Ledger.Alonzo.TxWitness qualified as C
import Cardano.Ledger.Alonzo.Data qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as C
import PlutusTx.Builtins.Internal qualified as P
import Plutus.V2.Ledger.Api qualified as P
import Plutus.V2.Ledger.Tx qualified as P
import Plutus.V2.Ledger.Tx qualified as Plutus hiding (TxIn(..), TxInType(..))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.Model.Fork.Ledger.Slot qualified as P
import Plutus.Model.Fork.TxExtra qualified as P
import Plutus.Model.Fork.Ledger.Tx qualified as Plutus
import Plutus.Model.Ada qualified as Ada
import PlutusTx.Builtins qualified as PlutusTx
import Plutus.Model.Fork.Ledger.Scripts qualified as C

type ToCardanoError = String

fromTxId :: C.TxId StandardCrypto -> P.TxId
fromTxId (C.TxId safeHash) =
  case extractHash safeHash of
    Crypto.UnsafeHash shortBs -> P.TxId $ P.BuiltinByteString $ fromShort shortBs

getInputsBy :: (Plutus.Tx -> Set.Set Plutus.TxIn) -> P.Tx -> Either ToCardanoError (Set.Set (C.TxIn StandardCrypto))
getInputsBy extract =
    fmap Set.fromList
  . mapM toTxIn
  . fmap Plutus.txInRef
  . Set.toList
  . extract
  . P.tx'plutus

toCoin :: Ada.Ada -> C.Coin
toCoin = C.Coin . Ada.getLovelace

getFee :: P.Tx -> C.Coin
getFee = toCoin . Plutus.txFee . P.tx'plutus

getInterval :: P.Tx -> C.ValidityInterval
getInterval = toInterval . Plutus.txValidRange . P.tx'plutus

getMint :: P.Tx -> Either ToCardanoError (C.Value StandardCrypto)
getMint = toValue . Plutus.txMint . P.tx'plutus

getDCerts :: Network -> C.Coin -> C.Coin -> P.Tx -> Either ToCardanoError (Seq.StrictSeq (C.DCert StandardCrypto))
getDCerts network poolDeposit minPoolCost =
    fmap Seq.fromList
  . mapM (toDCert network poolDeposit minPoolCost . P.certificate'dcert)
  . P.extra'certificates
  . P.tx'extra

getWdrl :: Network -> P.Tx -> Either ToCardanoError (C.Wdrl StandardCrypto)
getWdrl network =
    toWdrl network
  . P.extra'withdraws
  . P.tx'extra

getSignatories :: P.Tx -> Set.Set (C.KeyHash 'C.Witness StandardCrypto)
getSignatories =
    Set.fromList
  . fmap (C.hashKey . C.vKey)
  . Map.elems
  . Plutus.txSignatures
  . P.tx'plutus

toValue :: P.Value -> Either ToCardanoError (C.Value StandardCrypto)
toValue val = C.valueFromList totalAda <$> traverse fromValue vs
  where
    (totalAda, vs) = foldForAda $ Value.flattenValue val

    foldForAda = L.foldl' go (0, [])
      where
        go (ada, rest) coin@(cs, tok, amount)
          | cs == Value.adaSymbol && tok == Value.adaToken = (ada + amount, rest)
          | otherwise = (ada, coin : rest)

    fromValue (cs, tok, amount) = (, assetName, amount) <$> toPolicyId cs
      where
        assetName = toAssetName tok

toAssetName :: P.TokenName -> C.AssetName
toAssetName (P.TokenName bs) = C.AssetName $ toShort $ PlutusTx.fromBuiltin bs

toPolicyId :: P.CurrencySymbol -> Either ToCardanoError (C.PolicyID StandardCrypto)
toPolicyId (P.CurrencySymbol bs) = fmap C.PolicyID $ toScriptHash (P.ValidatorHash bs)

toScriptHash :: P.ValidatorHash -> Either ToCardanoError (C.ScriptHash StandardCrypto)
toScriptHash (P.ValidatorHash bs) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toScriptHash (" <> show (BS.length bsx) <> " bytes)"
  in  tag tg $ maybe (Left "Failed to get Script hash") Right $ C.ScriptHash <$> Crypto.hashFromBytes bsx

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (\x -> "tag " <> s <> " :" <> x)

toDCert :: Network -> C.Coin -> C.Coin -> P.DCert -> Either ToCardanoError (C.DCert StandardCrypto)
toDCert network poolDeposit minPoolCost = \case
  P.DCertDelegRegKey (P.StakingHash stakingCredential) -> C.DCertDeleg . C.RegKey <$> toCredential stakingCredential
  P.DCertDelegDeRegKey (P.StakingHash stakingCredential) -> C.DCertDeleg . C.DeRegKey <$> toCredential stakingCredential
  P.DCertDelegDelegate (P.StakingHash stakingCredential) pubKeyHash -> C.DCertDeleg . C.Delegate <$> (Delegation <$> toCredential stakingCredential <*> toPubKeyHash pubKeyHash)
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
                poolDeposit
                minPoolCost
                def
                (C.RewardAcnt network (C.KeyHashObj cpkh3))
                (Set.singleton cpkh2)
                Seq.empty
                C.SNothing

toTxIn :: P.TxOutRef -> Either ToCardanoError (C.TxIn StandardCrypto)
toTxIn (P.TxOutRef txId n) = (\tid -> C.TxIn tid (toEnum $ fromInteger n)) <$> toTxId txId

toTxId :: P.TxId -> Either ToCardanoError (C.TxId StandardCrypto)
toTxId (P.TxId bs) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toTxIdHash (" <> show (BS.length bsx) <> " bytes)"
  in tag tg $ maybe (Left "Failed to get TxId Hash") Right $ C.TxId . unsafeMakeSafeHash <$> Crypto.hashFromBytes bsx

toVrf :: P.PubKeyHash -> Either ToCardanoError (Shelley.Hash StandardCrypto (C.VerKeyVRF StandardCrypto))
toVrf (P.PubKeyHash bs) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toVrfHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ maybe (Left "Failed to get VRF Hash") Right $ Crypto.hashFromBytes bsx

toPubKeyHash :: P.PubKeyHash -> Either ToCardanoError (C.KeyHash kr StandardCrypto)
toPubKeyHash (P.PubKeyHash bs) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toPubKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ maybe (Left "Failed to get Hash") Right $ C.KeyHash <$> Crypto.hashFromBytes bsx

toCredential :: P.Credential -> Either ToCardanoError (C.Credential a StandardCrypto)
toCredential = \case
  P.PubKeyCredential pubKeyHash    -> C.KeyHashObj <$> toPubKeyHash pubKeyHash
  P.ScriptCredential validatorHash -> C.ScriptHashObj <$> toScriptHash validatorHash

toWdrl :: Network -> [P.Withdraw] -> Either ToCardanoError (C.Wdrl StandardCrypto)
toWdrl network ws = C.Wdrl . Map.fromList <$> mapM to ws
  where
    to (P.Withdraw scred amount _) =
      case scred of
        P.StakingHash cred -> (\x -> (C.RewardAcnt network x, C.Coin amount)) <$> toCredential cred
        _                  -> Left "Not supported staking cred in withdraw"

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

toDataHash :: P.DatumHash -> Either ToCardanoError (C.DataHash StandardCrypto)
toDataHash (P.DatumHash bs) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toDatumHash (" <> show (BS.length bsx) <> " bytes)"
  in tag tg $ maybe (Left "Failed to get TxId Hash") Right $ unsafeMakeSafeHash <$> Crypto.hashFromBytes bsx


toStrictMaybe :: Maybe a -> C.StrictMaybe a
toStrictMaybe = maybe C.SNothing C.SJust

toAddr :: Network -> P.Address -> Either ToCardanoError (C.Addr StandardCrypto)
toAddr network (P.Address addressCredential addressStakingCredential) =
  C.Addr network <$> toCredential addressCredential <*> toStakeAddressReference addressStakingCredential

toStakeAddressReference :: Maybe P.StakingCredential -> Either ToCardanoError (C.StakeReference StandardCrypto)
toStakeAddressReference = \case
  Nothing -> pure C.StakeRefNull
  Just (P.StakingHash stakeCred) -> C.StakeRefBase <$> toCredential stakeCred
  Just (P.StakingPtr x y z)      -> pure $ C.StakeRefPtr $ C.Ptr (C.SlotNo $ fromIntegral x) (TxIx $ fromIntegral y) (CertIx $ fromIntegral z)

toKeyWitness ::
     SafeHash StandardCrypto C.EraIndependentTxBody
  -> P.Tx
  -> Set (WitVKey 'C.Witness StandardCrypto)
toKeyWitness txBodyHash tx =
  Set.fromList $ fmap (C.makeWitnessVKey txBodyHash) $ Map.elems $ Plutus.txSignatures $ P.tx'plutus tx

toDatumWitness :: (C.Era era, C.Crypto era ~ StandardCrypto) => P.Tx -> Either ToCardanoError (C.TxDats era)
toDatumWitness tx = do
  datumWits1 <- fmap Map.fromList $ mapM (\d -> (, toDatum d) <$> (toDataHash $ C.datumHash d)) validatorDatums1
  datumWits2 <- fmap Map.fromList $ mapM (\(dh, d) -> (, toDatum d) <$> toDataHash dh) validatorDatums2
  pure $ C.TxDats $ datumWits1 <> datumWits2
  where
    validatorDatums1 = fmap (\(_,_,datum) -> datum) validatorInfo
    validatorDatums2 = Map.toList $ Plutus.txData $ P.tx'plutus tx

    validatorInfo = mapMaybe (fromInType <=< Plutus.txInType) (Set.toList $ Plutus.txInputs $ P.tx'plutus tx)

fromInType :: Plutus.TxInType -> Maybe (Maybe (C.Versioned P.Script), P.Redeemer, P.Datum)
fromInType = \case
  Plutus.ConsumeScriptAddress script redeemer datum -> Just (fmap P.getValidator <$> script, redeemer, datum)
  _ -> Nothing

toRedeemerWitness :: (C.Era era) => P.Tx -> C.Redeemers era
toRedeemerWitness tx =
  C.Redeemers $ mintRedeemers <> inputRedeemers <> certRedeemers <> withdrawRedeemers
  where
    mintRedeemers =
      Map.fromList
      $ fmap (\(P.RedeemerPtr _tag n, redm) -> (C.RdmrPtr C.Mint (fromInteger n), addDefaultExUnits $ toRedeemer redm))
      $ filter (isMint . fst) $ Map.toList $ Plutus.txRedeemers $ P.tx'plutus tx
      where
        isMint = \case
          P.RedeemerPtr Plutus.Mint _ -> True
          _                           -> False

    inputRedeemers =
      Map.fromList
      $ mapMaybe toInput
      $ zip [0..] $ Set.toList
      $ Plutus.txInputs $ P.tx'plutus tx
      where
        toInput (n, tin) =
          case  Plutus.txInType tin of
            Just (Plutus.ConsumeScriptAddress _validator redeemer _datum) ->
              Just (C.RdmrPtr C.Spend (fromInteger n), addDefaultExUnits $ toRedeemer redeemer)
            _ -> Nothing

    certRedeemers = redeemersBy C.Cert (fmap P.certificate'script . P.extra'certificates)
    withdrawRedeemers = redeemersBy C.Rewrd (fmap P.withdraw'script . P.extra'withdraws)

    redeemersBy :: C.Tag -> (P.Extra -> [Maybe (P.Redeemer, a)]) -> Map.Map C.RdmrPtr (C.Data era, C.ExUnits)
    redeemersBy scriptTag extract =
      Map.fromList
      $ mapMaybe toWithdraw
      $ zip [0..]
      $ extract $ P.tx'extra tx
      where
        toWithdraw (n, ws) = flip fmap ws $ \(redeemer, _script) ->
          (C.RdmrPtr scriptTag (fromInteger n), addDefaultExUnits $ toRedeemer redeemer)

    addDefaultExUnits rdm = (rdm, C.ExUnits 1 1)

toScriptWitness :: (C.Crypto era ~ StandardCrypto)
  => P.Tx -> Either ToCardanoError (Map (C.ScriptHash (C.Crypto era)) (C.Script era))
toScriptWitness tx =
  fmap Map.fromList $ mapM (\s -> (, C.toScript s) <$> toScriptHash (C.validatorHash (fmap P.Validator s))) allScripts
  where
    allScripts = mints <> withdraws <> validators <> certificates
      where
        mints = fmap (fmap P.getMintingPolicy) $ Set.toList $ Plutus.txMintScripts $ P.tx'plutus tx
        withdraws = mapMaybe (fmap (fmap P.getStakeValidator . snd) . P.withdraw'script) (P.extra'withdraws $ P.tx'extra tx)
        certificates = mapMaybe (fmap (fmap P.getStakeValidator . snd) . P.certificate'script) (P.extra'certificates $ P.tx'extra tx)
        validators = mapMaybe (\(script, _, _) -> script) validatorInfo

    validatorInfo = mapMaybe (fromInType <=< Plutus.txInType) (Set.toList $ Plutus.txInputs $ P.tx'plutus tx)

toDatum :: P.Datum -> C.Data era
toDatum (P.Datum (P.BuiltinData d)) = C.Data d

toRedeemer :: P.Redeemer -> C.Data era
toRedeemer (P.Redeemer (P.BuiltinData d)) = C.Data d

