{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Simple test model for plutus scripts.

 We can create blockchain with main user that holds all the value
 and can distribute it to test users.

 The blockchain update happens according to the Cardano node rules
 and is executed as simple state update. We can query all blockchain stats
 for the users.

 Also it estimates execution of the TXs accordig to cardano model.
-}
module Plutus.Test.Model.Blockchain (
  -- * Address helpers
  HasAddress(..),
  -- * Blockchain model
  Blockchain (..),
  BchConfig (..),
  CheckLimits(..),
  BchNames (..),
  User (..),
  TxStat (..),
  ExecutionUnits (..),
  Result (..),
  isOkResult,
  FailReason (..),
  LimitOverflow (..),
  modifyBchNames,
  writeUserName,
  writeAddressName,
  writeAssetClassName,
  writeCurrencySymbolName,
  readUserName,
  readAddressName,
  readAssetClassName,
  readCurrencySymbolName,
  Run (..),
  runBch,
  initBch,
  Percent(..),
  toPercent,
  StatPercent(..),
  PercentExecutionUnits(..),
  toStatPercent,

  -- * core blockchain functions
  getMainUser,
  signTx,
  sendBlock,
  sendTx,
  logFail,
  logInfo,
  logError,
  pureFail,
  txOutRefAt,
  getTxOut,
  utxoAt,
  datumAt,
  waitNSlots,
  getUserPubKey,

  -- * Blockchain config
  readBchConfig,
  readProtocolParameters,
  defaultBchConfig,
  readDefaultBchConfig,
  skipLimits,
  warnLimits,
  forceLimits,

  -- * Resources limits (Alonzo)
  mainnetBlockLimits,
  mainnetTxLimits,
  testnetBlockLimits,
  testnetTxLimits,

  -- * Logs
  Log(..),
  appendLog,
  nullLog,
  fromLog,
  fromGroupLog,
  BchEvent(..),
  silentLog,
  failLog,
  filterSlot,
  getLog,
  getFails,

  -- * internal
  intToPubKey,
) where

import Prelude

import Data.Aeson (decodeFileStrict')
import Data.ByteString qualified as BS
import Data.Either
import Data.Foldable
import Data.Function (on)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq

import Cardano.Api.Shelley (
  AlonzoEra,
  ConsensusMode (..),
  EraHistory (..),
  EraInMode (..),
  ExecutionUnits (..),
  NetworkId (..),
  ProtocolParameters (..),
  ScriptExecutionError,
  TransactionValidityIntervalError,
  UTxO (..),
  evaluateTransactionBalance,
  evaluateTransactionExecutionUnits,
  fromAlonzoData,
  serialiseToCBOR,
  toCtxUTxOTxOut,
  txOutValueToValue,
 )
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (RelativeTime (..), SystemStart (..), slotLengthFromMillisec)
import Control.Monad.State.Strict
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval ()
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Slot (Slot (..), SlotRange)
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.Prelude qualified as Plutus
import Basement.Compat.Natural
import qualified Plutus.V1.Ledger.Ada            as Ada
import Ledger.Typed.Scripts (TypedValidator, validatorAddress)
import Ledger (PaymentPubKeyHash(..))

import Ouroboros.Consensus.Block.Abstract (EpochNo (..), EpochSize (..))
import Ouroboros.Consensus.HardFork.History.EraParams
import Ouroboros.Consensus.HardFork.History.Qry (mkInterpreter)
import Ouroboros.Consensus.HardFork.History.Summary (
  Bound (..),
  EraEnd (..),
  EraSummary (..),
  Summary (..),
 )
import Ouroboros.Consensus.Util.Counting (NonEmpty (..))

import Cardano.Api qualified as Cardano
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Hashes as Ledger (EraIndependentTxBody)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Ledger.Crypto (PubKey (..), Signature (..), pubKeyHash)
import Ledger.TimeSlot (SlotConfig (..))
import Ledger.Tx.CardanoAPI qualified as Cardano
import Paths_plutus_simple_model

class HasAddress a where
  toAddress :: a -> Address

instance HasAddress Address where
  toAddress = id

instance HasAddress PubKeyHash where
  toAddress = pubKeyHashAddress

instance HasAddress ValidatorHash where
  toAddress = scriptHashAddress

instance HasAddress (TypedValidator a) where
  toAddress = validatorAddress

instance Semigroup ExecutionUnits where
  (<>) (ExecutionUnits a1 b1) (ExecutionUnits a2 b2) =
    ExecutionUnits (a1 + a2) (b1 + b2)

instance Monoid ExecutionUnits where
  mempty = ExecutionUnits 0 0

data PercentExecutionUnits = PercentExecutionUnits
  { percentExecutionSteps  :: !Percent
  , percentExecutionMemory :: !Percent
  }
  deriving (Show, Eq)

newtype User = User
  { userPubKey :: PubKey
  }
  deriving (Show)

-- | TX with stats of TX execution onchain.
data TxStat = TxStat
  { txStatTx        :: !Tx
  , txStatTime      :: !Slot
  , txStat          :: !Stat
  , txStatPercent   :: !StatPercent
  }
  deriving (Show)

-- | Config for the blockchain.
data BchConfig = BchConfig
  { bchConfigCheckLimits  :: !CheckLimits             -- ^ limits check mode
  , bchConfigLimitStats   :: !Stat                    -- ^ TX execution resources limits
  , bchConfigProtocol     :: !ProtocolParameters      -- ^ Protocol parameters
  , bchConfigNetworkId    :: !NetworkId               -- ^ Network id (mainnet / testnet)
  , bchConfigSlotConfig   :: !SlotConfig              -- ^ Slot config
  }

data CheckLimits
  = IgnoreLimits   -- ^ ignore TX-limits
  | WarnLimits     -- ^ log TX to error log if it exceeds limits but accept TX
  | ErrorLimits    -- ^ reject TX if it exceeds the limits
  deriving (Show)

-- | Default slot config
defaultSlotConfig :: SlotConfig
defaultSlotConfig =
  SlotConfig
    { scSlotLength = 1000 -- each slot lasts for 1 second
    , scSlotZeroTime = 0 -- starts at unix epoch start
    }

-- | Loads default config for the blockchain. Uses presaved era history and protocol parameters.
readDefaultBchConfig :: IO BchConfig
readDefaultBchConfig = do
  paramsFile <- getDataFileName "data/protocol-params.json"
  readBchConfig paramsFile

-- | Default blockchain config.
defaultBchConfig :: ProtocolParameters -> BchConfig
defaultBchConfig params =
  BchConfig
    { bchConfigLimitStats = mainnetTxLimits
    , bchConfigCheckLimits = ErrorLimits
    , bchConfigProtocol = params
    , bchConfigNetworkId = Mainnet
    , bchConfigSlotConfig = defaultSlotConfig
    }

-- | Do not check for limits
skipLimits :: BchConfig -> BchConfig
skipLimits cfg = cfg { bchConfigCheckLimits = IgnoreLimits }

-- | Warn on limits
warnLimits :: BchConfig -> BchConfig
warnLimits cfg = cfg { bchConfigCheckLimits = WarnLimits }

-- | Error on limits
forceLimits :: BchConfig -> BchConfig
forceLimits cfg = cfg { bchConfigCheckLimits = ErrorLimits }

{- | Read config for protocol parameters and form blockchain config.

 > readBchConfig protocolParametersFile
-}
readBchConfig :: FilePath -> IO BchConfig
readBchConfig paramsFile =
  defaultBchConfig <$> readProtocolParameters paramsFile

-- | Reads protocol parameters from file.
readProtocolParameters :: FilePath -> IO ProtocolParameters
readProtocolParameters file =
  fmap fromJust $ decodeFileStrict' file

-- | Stats of TX execution onchain.
data Stat = Stat
  { statSize           :: !Integer          -- ^ TX-size in bytes
  , statExecutionUnits :: !ExecutionUnits   -- ^ execution units of TX
  }
  deriving (Show, Eq)

-- | Percent values from 0 to 100 %.
newtype Percent = Percent { getPercent :: Float }
  deriving (Show, Eq)

-- | Convert integer to percent based on maximum value (first argument)
toPercent :: Integer -> Integer -> Percent
toPercent maxLim n = Percent $ (fromInteger @Float $ 100 * n ) / fromInteger maxLim

-- | Stats measured in percents (0 to 100 %)
data StatPercent = StatPercent
  { statPercentSize           :: !Percent
  , statPercentExecutionUnits :: !PercentExecutionUnits
  }
  deriving (Show, Eq)

-- | Get Stats expressed in percents based on maximum limits and given stats.
toStatPercent :: Stat -> Stat -> StatPercent
toStatPercent maxStat stat =
  StatPercent
    { statPercentSize = percent statSize
    , statPercentExecutionUnits = PercentExecutionUnits
        { percentExecutionSteps  = percentNat executionSteps
        , percentExecutionMemory = percentNat executionMemory
        }
    }
  where
    percentNat getter = percent (naturalToInteger . getter . statExecutionUnits)

    percent :: (Stat -> Integer) -> Percent
    percent getter = toPercent (getter maxStat) (getter stat)

{- | Simple model for UTXO-based blockchain.
 We have set of UTXOs. Every UTXO can belong to the user (owner of PubKey) or to script.
 We submit blocks of TXs to update the blockchain. TX destroys input UTXOs and produces new UTXOs.

 Update happens as pure function in the State-monad. As TX is submitted we get useful performance stats
 such as TX-size and execution units. All stats are calculated with cardano node functions and TX-size
 is estimated on Cardano version of TX.
-}
data Blockchain = Blockchain
  { bchUsers        :: !(Map PubKeyHash User)
  , bchAddresses    :: !(Map Address (Set TxOutRef))
  , bchUtxos        :: !(Map TxOutRef TxOut)
  , bchDatums       :: !(Map DatumHash Datum)
  , bchTxs          :: !(Log TxStat)
  , bchConfig       :: !BchConfig
  , bchCurrentSlot  :: !Slot
  , bchUserStep     :: !Integer
  , bchFails        :: !(Log FailReason)
  , bchInfo         :: !(Log String)
  , -- | human readable names. Idea is to substitute for them
    -- in pretty printers for error logs, user names, script names.
    bchNames :: !BchNames
  }

newtype Log a = Log { unLog :: Seq (Slot, a) }
  deriving (Functor)

instance Semigroup (Log a) where
  (<>) (Log sa) (Log sb) = Log (merge sa sb)
    where
      merge Empty b = b
      merge a Empty = a
      merge (a :<| as) (b :<| bs) =
        if fst a <= fst b
          then a Seq.<| merge as (b Seq.<| bs)
          else b Seq.<| merge (a Seq.<| as) bs

appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

nullLog :: Log a -> Bool
nullLog (Log a) = Seq.null a

fromLog :: Log a -> [(Slot, a)]
fromLog (Log s) = toList s

fromGroupLog :: Log a -> [(Slot, [a])]
fromGroupLog = fmap toGroup . L.groupBy ((==) `on` fst) . fromLog
  where
    toGroup ((a, b) : rest) = (a, b : fmap snd rest)
    toGroup [] = error "toGroup: Empty list"

instance Monoid (Log a) where
  mempty = Log Seq.empty

-- | Result of the execution.
data Result = Ok | Fail FailReason
  deriving (Show)

-- | Result is ok.
isOkResult :: Result -> Bool
isOkResult = \case
  Ok -> True
  _ -> False

-- | Fail reasons.
data FailReason
  = -- | use with given pub key hash is not found. User was not registered with @newUser@ or @newUserWith@.
    NoUser PubKeyHash
  | -- | not enough funds for the user.
    NotEnoughFunds PubKeyHash Value
  | -- | time or vlaid range related errors
    IntervalError TransactionValidityIntervalError
  | -- | TX is not balanced. Sum of inputs does not equal to sum of outputs.
    NotBalancedTx
  | -- | no utxo on the address
    FailToReadUtxo
  | -- | failed to convert plutus TX to cardano TX. TX is malformed.
    FailToCardano Cardano.ToCardanoError
  | -- | execution of the script failure
    TxScriptFail [ScriptExecutionError]
  | -- | invalid range. TX is submitted with current slot not in valid range
    TxInvalidRange Slot SlotRange
  | TxLimitError [LimitOverflow] StatPercent
  -- | Any error (can be useful to report logic errors on testing)
  | GenericFail String
  deriving (Show)

-- | Encodes overflow of the TX-resources
data LimitOverflow
  = TxSizeError !Integer !Percent -- ^ by how many bytes we exceed the limit
  | ExMemError !Integer !Percent  -- ^ how many mem units exceeded
  | ExStepError !Integer !Percent -- ^ how many steps executions exceeded
  deriving (Show, Eq)

-- | State monad wrapper to run blockchain.
newtype Run a = Run (State Blockchain a)
  deriving (Functor, Applicative, Monad, MonadState Blockchain)

-- | Human readable names for pretty printing.
data BchNames = BchNames
  { bchNameUsers :: !(Map PubKeyHash String)
  , bchNameAddresses :: !(Map Address String)
  , bchNameAssetClasses :: !(Map AssetClass String)
  , bchNameCurrencySymbols :: !(Map CurrencySymbol String)
  }

-- | Modifies the mappings to human-readable names
modifyBchNames :: (BchNames -> BchNames) -> Run ()
modifyBchNames f = modify' $ \s -> s {bchNames = f (bchNames s)}

-- | Assigns human-readable name to user
writeUserName :: PubKeyHash -> String -> Run ()
writeUserName pkh name = do
  modifyBchNames $ \ns ->
    ns {bchNameUsers = M.insert pkh name (bchNameUsers ns)}
  writeAddressName (pubKeyHashAddress pkh) name

-- | Assigns human-readable name to address
writeAddressName :: Address -> String -> Run ()
writeAddressName addr name = modifyBchNames $ \ns ->
   ns {bchNameAddresses = M.insert addr name (bchNameAddresses ns)}

-- | Assigns human-readable name to asset class
writeAssetClassName :: AssetClass -> String -> Run ()
writeAssetClassName ac name = modifyBchNames $ \ns ->
  ns {bchNameAssetClasses = M.insert ac name (bchNameAssetClasses ns)}

-- | Assigns human-readable name to currency symbol
writeCurrencySymbolName :: CurrencySymbol -> String -> Run ()
writeCurrencySymbolName cs name = modifyBchNames $ \ns ->
  ns {bchNameCurrencySymbols = M.insert cs name (bchNameCurrencySymbols ns)}

-- | Gets human-readable name of user
readUserName :: BchNames -> PubKeyHash -> Maybe String
readUserName names pkh = M.lookup pkh (bchNameUsers names)

-- | Gets human-readable name of address
readAddressName :: BchNames -> Address -> Maybe String
readAddressName names addr = M.lookup addr (bchNameAddresses names)

-- | Gets human-readable name of user
readAssetClassName :: BchNames -> AssetClass -> Maybe String
readAssetClassName names ac = M.lookup ac (bchNameAssetClasses names)

-- | Gets human-readable name of user
readCurrencySymbolName :: BchNames -> CurrencySymbol -> Maybe String
readCurrencySymbolName names cs = M.lookup cs (bchNameCurrencySymbols names)

--------------------------------------------------------
-- API

{- | Get pub key hash of the admin user.
 It can be useful to distribute funds to the users.
-}
getMainUser :: Run PubKeyHash
getMainUser = pure $ pubKeyHash $ intToPubKey 0

-- | Run blockchain.
runBch :: Run a -> Blockchain -> (a, Blockchain)
runBch (Run act) = runState act

-- | Init blockchain state.
initBch :: BchConfig -> Value -> Blockchain
initBch cfg initVal =
  Blockchain
    { bchUsers = M.singleton genesisUserId genesisUser
    , bchUtxos = M.singleton genesisTxOutRef genesisTxOut
    , bchDatums = M.empty
    , bchAddresses = M.singleton genesisAddress (S.singleton genesisTxOutRef)
    , bchTxs = mempty
    , bchConfig = cfg
    , bchCurrentSlot = Slot 1
    , bchUserStep = 1
    , bchFails = mempty
    , bchInfo = mempty
    , bchNames = BchNames
                  (M.singleton genesisUserId "Genesis role")
                  (M.singleton genesisAddress "Genesis role")
                  M.empty
                  M.empty
    }
  where
    genesisUserId = pubKeyHash genesisPubKey
    genesisPubKey = intToPubKey 0
    genesisUser = User genesisPubKey
    genesisAddress = pubKeyHashAddress genesisUserId

    genesisTxOutRef = TxOutRef genesisTxId 0
    genesisTxOut = TxOut (pubKeyHashAddress genesisUserId) initVal Nothing

-- Hash for genesis transaction
dummyHash :: Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody
dummyHash = Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

-- | genesis transaction ID
genesisTxId :: TxId
genesisTxId = Cardano.fromCardanoTxId . Cardano.TxId $ dummyHash

intToPubKey :: Integer -> PubKey
intToPubKey n = PubKey $ LedgerBytes $ Plutus.sha2_256 $ Plutus.consByteString n Plutus.mempty

getUserPubKey :: PubKeyHash -> Run (Maybe PubKey)
getUserPubKey pkh =
  fmap userPubKey . M.lookup pkh <$> gets bchUsers

-- | Sign TX for the user.
signTx :: PubKeyHash -> Tx -> Run Tx
signTx pkh tx = do
  mPk <- getUserPubKey pkh
  case mPk of
    Just pk -> pure $ tx {txSignatures = M.insert pk (Signature $ getPubKeyHash pkh) $ txSignatures tx}
    Nothing -> do
      logFail (NoUser pkh)
      pure tx

-- | Return list of failures
getFails :: Run (Log FailReason)
getFails = gets bchFails

-- | Logs failure and returns it.
pureFail :: FailReason -> Run Result
pureFail res = do
  logFail res
  pure $ Fail res

-- | Log failure.
logFail :: FailReason -> Run ()
logFail res = do
  curTime <- gets bchCurrentSlot
  modify' $ \s -> s {bchFails = appendLog curTime res (bchFails s) }

-- | Log generic error.
logError :: String -> Run ()
logError = logFail . GenericFail

logInfo :: String -> Run ()
logInfo msg = do
  slot <- gets bchCurrentSlot
  modify' $ \s -> s { bchInfo = appendLog slot msg (bchInfo s) }

-- | Send block of TXs to blockchain.
sendBlock :: [Tx] -> Run (Either FailReason [Stat])
sendBlock txs = do
  res <- sequence <$> mapM sendSingleTx txs
  when (isRight res) bumpSlot
  pure res

-- | Sends block with single TX to blockchai
sendTx :: Tx -> Run (Either FailReason Stat)
sendTx tx = do
  res <- sendSingleTx tx
  when (isRight res) bumpSlot
  pure res

{- | Send single TX to blockchain. It logs failure if TX is invalid
 and pproduces performance stats if TX was ok.
-}
sendSingleTx :: Tx -> Run (Either FailReason Stat)
sendSingleTx tx =
  withCheckRange $
    withTxBody $ \protocol txBody -> do
      let tid = Cardano.fromCardanoTxId $ Cardano.getTxId txBody
      withUTxO tid $ \utxo ->
        withCheckBalance protocol utxo txBody $
          withCheckUnits protocol utxo txBody $ \cost -> do
            let txSize = fromIntegral $ BS.length $ serialiseToCBOR txBody
                stat = Stat txSize cost
            withCheckTxLimits stat $ do
              applyTx stat tid tx
              pure $ Right stat
  where
    pkhs = fmap pubKeyHash $ M.keys $ txSignatures tx

    withCheckRange cont = do
      curSlot <- gets bchCurrentSlot
      if Interval.member curSlot $ txValidRange tx
        then cont
        else leftFail $ TxInvalidRange curSlot (txValidRange tx)

    withUTxO tid cont = do
      mUtxo <- getUTxO tid tx
      case mUtxo of
        Just (Right utxo) -> cont utxo
        Just (Left err) -> leftFail $ FailToCardano err
        Nothing -> leftFail FailToReadUtxo

    withTxBody cont = do
      cfg <- gets bchConfig
      case Cardano.toCardanoTxBody (fmap PaymentPubKeyHash pkhs) (Just $ bchConfigProtocol cfg) (bchConfigNetworkId cfg) tx of
        Right txBody -> cont (bchConfigProtocol cfg) txBody
        Left err -> leftFail $ FailToCardano err

    withCheckBalance protocol utxo txBody cont
      | balanceIsOk = cont
      | otherwise = leftFail NotBalancedTx
      where
        balanceIsOk = txOutValueToValue (evaluateTransactionBalance protocol S.empty utxo txBody) == mempty

    withCheckUnits protocol utxo txBody cont = do
      slotCfg <- gets (bchConfigSlotConfig . bchConfig)
      let cardanoSystemStart = SystemStart $ posixSecondsToUTCTime $ fromInteger $ (`div` 1000) $ getPOSIXTime $ scSlotZeroTime slotCfg
          -- see EraSummary: http://localhost:8080/file//nix/store/qix63dnd40m23iap66184b4vib426r66-ouroboros-consensus-lib-ouroboros-consensus-0.1.0.0-haddock-doc/share/doc/ouroboros-consensus/html/Ouroboros-Consensus-HardFork-History-Summary.html#t:EraSummary
          eStart = Bound (RelativeTime 0) (SlotNo 0) (EpochNo 0)
          eEnd = EraUnbounded
          eParams = EraParams (EpochSize 1) (slotLengthFromMillisec $ scSlotLength slotCfg) (StandardSafeZone 1)
          eraHistory = EraHistory CardanoMode $ mkInterpreter $ Summary $ NonEmptyOne $ EraSummary eStart eEnd eParams
      case getExecUnits cardanoSystemStart eraHistory of
        Right res ->
          let res' = (\(k, v) -> fmap (k,) v) <$> M.toList res
              errs = foldErrors res'
              cost = foldCost res'
           in case errs of
                [] -> cont cost
                _ -> leftFail $ TxScriptFail errs
        Left err -> leftFail $ IntervalError err
      where
        getExecUnits sysStart eraHistory = evaluateTransactionExecutionUnits AlonzoEraInCardanoMode sysStart eraHistory protocol utxo txBody
        foldErrors = lefts
        foldCost = foldMap snd . rights

    withCheckTxLimits stat cont = do
      maxLimits <- gets (bchConfigLimitStats . bchConfig)
      checkLimits <- gets (bchConfigCheckLimits . bchConfig)
      let errs = compareLimits maxLimits stat
          statPercent = toStatPercent maxLimits stat
      if null errs
        then cont
        else
          case checkLimits of
            IgnoreLimits -> cont
            WarnLimits   -> logFail (TxLimitError errs statPercent) >> cont
            ErrorLimits  -> leftFail (TxLimitError errs statPercent)

    leftFail err = do
      logFail err
      pure $ Left err

compareLimits :: Stat -> Stat -> [LimitOverflow]
compareLimits maxLimits stat = catMaybes
  [ cmp TxSizeError statSize
  , cmp ExMemError (naturalToInteger . executionMemory . statExecutionUnits)
  , cmp ExStepError (naturalToInteger . executionSteps . statExecutionUnits)
  ]
  where
    cmp cons getter
      | overflow > 0 = Just $ cons overflow (toPercent (getter maxLimits) overflow)
      | otherwise    = Nothing
      where
        overflow = getter stat - getter maxLimits


-- | Read UTxO relevant to transaction
getUTxO :: TxId -> Tx -> Run (Maybe (Either Cardano.ToCardanoError (UTxO AlonzoEra)))
getUTxO tid tx = do
  networkId <- bchConfigNetworkId <$> gets bchConfig
  mOuts <- sequence <$> mapM (getTxOut . txInRef) ins
  pure $ fmap (toUtxo networkId . zip ins) mOuts
  where
    ins = S.toList $ txInputs tx
    outs = zip [0..] $ txOutputs tx

    fromOutTxOut networkId (ix, tout) = do
      cin <- Cardano.toCardanoTxIn $ TxOutRef tid ix
      cout <- fmap toCtxUTxOTxOut $ Cardano.TxOut
                <$> Cardano.toCardanoAddress networkId (txOutAddress tout)
                <*> toCardanoTxOutValue (txOutValue tout)
                <*> pure (fromMaybe Cardano.TxOutDatumNone $ do
                       dh <- txOutDatumHash tout
                       dat <- M.lookup dh (txData tx)
                       pure $ Cardano.TxOutDatum Cardano.ScriptDataInAlonzoEra (toScriptData dat))
      pure (cin, cout)

    fromTxOut networkId (tin, tout) = do
      cin <- Cardano.toCardanoTxIn $ txInRef tin
      cout <- fmap toCtxUTxOTxOut $ Cardano.toCardanoTxOut networkId (txData tx) tout
      pure (cin, cout)

    toUtxo :: NetworkId -> [(TxIn, TxOut)] -> Either Cardano.ToCardanoError (UTxO AlonzoEra)
    toUtxo networkId xs = UTxO . M.fromList <$> (mappend <$> mapM (fromTxOut networkId) xs <*> mapM (fromOutTxOut networkId) outs)

toScriptData :: ToData a => a -> Cardano.ScriptData
toScriptData d = fromAlonzoData $ Alonzo.Data $ toData d

toCardanoTxOutValue :: Value -> Either Cardano.ToCardanoError (Cardano.TxOutValue Cardano.AlonzoEra)
toCardanoTxOutValue value = do
    when (Ada.fromValue value == mempty) (Left Cardano.OutputHasZeroAda)
    Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra <$> Cardano.toCardanoValue value

-- | Reads TxOut by its reference.
getTxOut :: TxOutRef -> Run (Maybe TxOut)
getTxOut ref = M.lookup ref <$> gets bchUtxos

bumpSlot :: Run ()
bumpSlot = modify' $ \s -> s {bchCurrentSlot = bchCurrentSlot s + 1}

-- | Makes slot counter of blockchain to move forward on given amount.
waitNSlots :: Slot -> Run ()
waitNSlots n = modify' $ \s -> s {bchCurrentSlot = bchCurrentSlot s + n}

-- | Applies valid TX to modify blockchain.
applyTx :: Stat -> TxId -> Tx -> Run ()
applyTx stat tid tx@Tx {..} = do
  updateUtxos
  saveTx
  saveDatums
  where
    saveDatums = modify' $ \s -> s {bchDatums = txData <> bchDatums s}

    saveTx = do
      t <- gets bchCurrentSlot
      statPercent <- getStatPercent
      modify' $ \s -> s {bchTxs = appendLog t (TxStat tx t stat statPercent) $ bchTxs s}

    getStatPercent = do
      maxLimits <- gets (bchConfigLimitStats . bchConfig)
      pure $ toStatPercent maxLimits stat

    updateUtxos = do
      removeIns txInputs
      mapM_ insertOut $ zip [0 ..] txOutputs

    removeIns ins = modify $ \s ->
      s
        { bchUtxos = rmIns (bchUtxos s)
        , bchAddresses = fmap (`S.difference` inRefSet) (bchAddresses s)
        }
      where
        inRefSet = S.map txInRef ins
        inRefs = M.fromList $ (,()) . txInRef <$> S.toList ins
        rmIns a = M.difference a inRefs

    insertOut (ix, out) = do
      insertAddresses
      insertUtxos
      where
        ref = TxOutRef tid ix
        addr = txOutAddress out

        insertAddresses = modify' $ \s -> s {bchAddresses = M.alter (Just . maybe (S.singleton ref) (S.insert ref)) addr $ bchAddresses s}
        insertUtxos = modify' $ \s -> s {bchUtxos = M.singleton ref out <> bchUtxos s}

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: Address -> Run [TxOutRef]
txOutRefAt addr = maybe [] S.toList . M.lookup addr <$> gets bchAddresses

-- | Get all UTXOs that belong to an address
utxoAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
utxoAt (toAddress -> addr) = do
  refs <- txOutRefAt addr
  fmap (\m -> mapMaybe (\r -> (r,) <$> M.lookup r m) refs) $ gets bchUtxos

-- | Reads typed datum from blockchain that belongs to UTXO (by reference).
datumAt :: FromData a => TxOutRef -> Run (Maybe a)
datumAt ref = do
  dhs <- gets bchDatums
  mDh <- (txOutDatumHash =<<) <$> getTxOut ref
  pure $ fromBuiltinData . getDatum =<< (`M.lookup` dhs) =<< mDh

---------------------------------------------------------------------
-- stat resources limits (Alonzo era)

-- | Limits for TX-execution resources on Mainnet (Alonzo)
mainnetTxLimits :: Stat
mainnetTxLimits =
  Stat
    { statSize  = 16 * 1024
    , statExecutionUnits = ExecutionUnits
        { executionMemory = 10_000_000
        , executionSteps = 10_000_000_000
        }
    }

-- | Limits for Block-execution resources resources on Mainnet
mainnetBlockLimits :: Stat
mainnetBlockLimits =
  Stat
    { statSize = 65 * 1024
    , statExecutionUnits = ExecutionUnits
      { executionMemory = 50_000_000
      , executionSteps = 40_000_000_000
      }
    }

-- | Limits for TX-execution resources resources on Testnet
testnetTxLimits :: Stat
testnetTxLimits = mainnetTxLimits

-- | Limits for Block-execution resources resources on Testnet
testnetBlockLimits :: Stat
testnetBlockLimits = mainnetBlockLimits

----------------------------------------------------------------
-- logs

-- | Blockchain events to log.
data BchEvent
  = BchTx TxStat           -- ^ Sucessful TXs
  | BchInfo String         -- ^ Info messages
  | BchFail FailReason     -- ^ Errors

-- | Skip all info messages
silentLog :: Log BchEvent -> Log BchEvent
silentLog (Log xs) = Log $ Seq.filter (not . isInfo . snd) xs
  where
    isInfo = \case
      BchInfo _ -> True
      _         -> False

-- | Skip successful TXs
failLog :: Log BchEvent -> Log BchEvent
failLog (Log xs) = Log $ Seq.filter (not . isTx . snd) xs
  where
    isTx = \case
      BchTx _ -> True
      _       -> False

-- | filter by slot. Can be useful to filter out unnecessary info.
filterSlot :: (Slot -> Bool) -> Log a -> Log a
filterSlot f (Log xs) = Log (Seq.filter (f . fst) xs)

-- | Reads the log.
getLog :: Blockchain -> Log BchEvent
getLog Blockchain{..} =
  mconcat [BchInfo <$> bchInfo, BchTx <$> bchTxs, BchFail <$> bchFails]

