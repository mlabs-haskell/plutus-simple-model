module Plutus.Test.Model.Fork.Cardano.Common(
  ToCardanoError,
  getInputsBy,
  getInterval,
  getFee,
  getMint,
  getDCerts,
  getWdrl,
  toValue,
  toTxIn,
  toAssetName,
  toPolicyId,
  toScriptHash,
  toCredential,
  toInterval,
  toSlot,
) where

import Prelude
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Short (toShort)
import Data.Default (def)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set

import Cardano.Ledger.SafeHash
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.API.Types qualified as C (
  StrictMaybe(..),
  Coin(..),
  PoolParams(..),
  RewardAcnt(..),
  Credential(..),
  VerKeyVRF,
  KeyHash(..),
  Wdrl(..)
  )
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.Slot qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.Delegation.Certificates qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as Shelley (Hash)
import Cardano.Ledger.TxIn qualified as C
import Cardano.Ledger.ShelleyMA.Timelocks qualified as C
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Ledger.Mary.Value qualified as C
import Cardano.Ledger.Hashes qualified as C
import Plutus.V2.Ledger.Api qualified as P
import Plutus.V2.Ledger.Tx qualified as P
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.Test.Model.Fork.Ledger.Slot qualified as P
import Plutus.Test.Model.Fork.TxExtra qualified as P
import Plutus.Test.Model.Fork.Ledger.Tx qualified as Plutus
import Plutus.Test.Model.Fork.Ledger.Ada qualified as Ada
import PlutusTx.Builtins qualified as PlutusTx

type ToCardanoError = String

getInputsBy :: (Plutus.Tx -> Set.Set P.TxIn) -> P.Tx -> Either ToCardanoError (Set.Set (C.TxIn StandardCrypto))
getInputsBy extract =
    fmap Set.fromList
  . mapM toTxIn
  . fmap P.txInRef
  . Set.toList
  . extract
  . P.tx'plutus

getFee :: P.Tx -> C.Coin
getFee = C.Coin . Ada.getLovelace . Plutus.txFee . P.tx'plutus

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

