{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Simple test model for plutus scripts.

 We can create blockchain with main user that holds all the value
 and can distribute it to test users.

 The blockchain update happens according to the Cardano node rules
 and is executed as simple state update. We can query all blockchain stats
 for the users.

 Also it estimates execution of the TXs accordig to cardano model.
-}
module Plutus.Model.Mock (
  -- * Address helpers
  HasAddress(..),
  HasStakingCredential(..),
  AppendStaking(..),
  appendStakingCredential,
  appendStakingPubKey,
  appendStakingScript,
  -- * Mock blockchain model
  Mock (..),
  MockConfig (..),
  CheckLimits(..),
  MockNames (..),
  User (..),
  TxStat (..),
  txStatId,
  PoolId(..),
  ExUnits (..),
  Result (..),
  isOkResult,
  FailReason (..),
  LimitOverflow (..),
  modifyMockNames,
  writeUserName,
  writeAddressName,
  writeAssetClassName,
  writeCurrencySymbolName,
  writeTxName,
  readUserName,
  readAddressName,
  readAssetClassName,
  readCurrencySymbolName,
  readTxName,
  getPrettyAddress,
  getPrettyCurrencySymbol,
  getPrettyAssetClass,
  getPrettyTxId,
  RunT (..),
  Run,
  runMock,
  runMock',
  initMock,
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
  noLog,
  noLogTx,
  noLogInfo,
  pureFail,
  txOutRefAt,
  getTxOut,
  withMay,
  withMayBy,
  utxoAt,
  refScriptAt,
  withUtxo,
  withFirstUtxo,
  withRefScript,
  withFirstRefScript,
  utxoAtStateBy,
  withDatum,
  datumAt,
  getHashDatum,
  getInlineDatum,
  txOutDatumHash,
  rewardAt,
  stakesAt,
  hasPool,
  hasStake,
  getPools,
  waitNSlots,

  -- * Blockchain config
  readMockConfig,
  defaultAlonzo,
  defaultBabbage,
  defaultMockConfig,
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
  MockEvent(..),
  silentLog,
  failLog,
  filterSlot,
  getLog,
  getFails,
  MustFailLog(..),

  -- * internal
  intToUser,
  userPubKeyHash,
) where

import Prelude
import Control.Applicative (Alternative(..))
import GHC.Records

import Control.Monad.Identity
import Data.ByteString qualified as BS
import Data.Either
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Text (Text)

import Data.List qualified as L
import Data.Vector qualified as V
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Sequence.Strict (StrictSeq)

import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO)
import Cardano.Ledger.Era (Era, Crypto)
import Cardano.Ledger.Alonzo.TxWitness qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C
import Cardano.Crypto.Seed qualified as C
import Cardano.Crypto.DSIGN.Class qualified as C
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.Crypto qualified as C
import Cardano.Slotting.EpochInfo.Impl (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), slotLengthFromMillisec)
import Control.Monad.State.Strict
import Plutus.V2.Ledger.Api hiding (Map)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.Model.Fork.Ledger.Tx qualified as P
import Plutus.Model.Fork.Ledger.Tx qualified as Plutus
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V1.Ledger.Address (pubKeyHashAddress, toPubKeyHash)
import Cardano.Ledger.Core qualified as Core
import GHC.Natural

import Cardano.Ledger.Hashes qualified as C
import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Hashes as Ledger (EraIndependentTxBody)
import Cardano.Ledger.SafeHash qualified as Ledger (unsafeMakeSafeHash)
import Plutus.Model.Fork.Ledger.Slot
import Plutus.Model.Fork.Ledger.TimeSlot (SlotConfig (..))
import Plutus.Model.Fork.TxExtra
import Plutus.Model.Stake
import Plutus.Model.Mock.ProtocolParameters

import Cardano.Ledger.Shelley.API.Wallet qualified as C
import Cardano.Ledger.SafeHash qualified as C
import Cardano.Ledger.TxIn qualified as Ledger
import Cardano.Ledger.Alonzo.Tools (evaluateTransactionExecutionUnits)
import Cardano.Ledger.Shelley.API.Wallet (evaluateTransactionBalance)
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Plutus.Model.Fork.Cardano.Alonzo ()
import Plutus.Model.Fork.Cardano.Babbage  ()
import Plutus.Model.Fork.Cardano.Common (fromTxId)
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import Plutus.Model.Ada (Ada(..))
import Plutus.Model.Mock.MockConfig
import Plutus.Model.Mock.Log
import Plutus.Model.Mock.Address
import Plutus.Model.Mock.Stat
import Plutus.Model.Fork.Cardano.Class qualified as Class
import Cardano.Ledger.Babbage.PParams
import Plutus.Model.Fork.Cardano.Alonzo qualified as Alonzo
import Plutus.Model.Fork.Cardano.Babbage qualified as Babbage

newtype User = User
  { userSignKey :: C.KeyPair 'C.Witness C.StandardCrypto
  }
  deriving (Show)

{- | Simple model for UTXO-based blockchain.
 We have set of UTXOs. Every UTXO can belong to the user (owner of PubKey) or to script.
 We submit blocks of TXs to update the blockchain. TX destroys input UTXOs and produces new UTXOs.

 Update happens as pure function in the State-monad. As TX is submitted we get useful performance stats
 such as TX-size and execution units. All stats are calculated with cardano node functions and TX-size
 is estimated on Cardano version of TX.
-}
data Mock = Mock
  { mockUsers        :: !(Map PubKeyHash User)
  , mockAddresses    :: !(Map Address (Set TxOutRef))
  , mockUtxos        :: !(Map TxOutRef TxOut)
  , mockRefScripts   :: !(Map TxOutRef TxOut)
  , mockDatums       :: !(Map DatumHash Datum)
  , mockStake        :: !Stake
  , mockTxs          :: !(Log TxStat)
  , mockConfig       :: !MockConfig
  , mockCurrentSlot  :: !Slot
  , mockUserStep     :: !Integer
  , mockFails        :: !(Log FailReason)
  , mockInfo         :: !(Log String)
  , mustFailLog     :: !(Log MustFailLog)
  , -- | human readable names. Idea is to substitute for them
    -- in pretty printers for error logs, user names, script names.
    mockNames :: !MockNames
  }

-- | Result of the execution.
data Result = Ok | Fail FailReason
  deriving (Show)

-- | Result is ok.
isOkResult :: Result -> Bool
isOkResult = \case
  Ok -> True
  _ -> False

-- | State monad wrapper to run blockchain.
newtype RunT m a = RunT (StateT Mock m a)
  deriving newtype (Functor, Applicative, Monad, MonadState Mock, MonadTrans)

-- | Dummy instance to be able to use partial pattern matching
-- in do-notation
instance Monad m => MonadFail (RunT m) where
   fail err = error $ "Failed to recover: " <> err

type Run a = RunT Identity a

-- | Human readable names for pretty printing.
data MockNames = MockNames
  { mockNameUsers :: !(Map PubKeyHash String)
  , mockNameAddresses :: !(Map Address String)
  , mockNameAssetClasses :: !(Map AssetClass String)
  , mockNameCurrencySymbols :: !(Map CurrencySymbol String)
  , mockNameTxns :: !(Map TxId String)
  }

-- | Modifies the mappings to human-readable names
modifyMockNames :: Monad m => (MockNames -> MockNames) -> RunT m ()
modifyMockNames f = modify' $ \s -> s {mockNames = f (mockNames s)}

-- | Assigns human-readable name to user
writeUserName :: Monad m => PubKeyHash -> String -> RunT m ()
writeUserName pkh name = do
  modifyMockNames $ \ns ->
    ns {mockNameUsers = M.insert pkh name (mockNameUsers ns)}
  writeAddressName (pubKeyHashAddress pkh) name

-- | Assigns human-readable name to address
writeAddressName :: Monad m => Address -> String -> RunT m ()
writeAddressName addr name = modifyMockNames $ \ns ->
   ns {mockNameAddresses = M.insert addr name (mockNameAddresses ns)}

-- | Assigns human-readable name to asset class
writeAssetClassName :: Monad m => AssetClass -> String -> RunT m ()
writeAssetClassName ac name = modifyMockNames $ \ns ->
  ns {mockNameAssetClasses = M.insert ac name (mockNameAssetClasses ns)}

-- | Assigns human-readable name to currency symbol
writeCurrencySymbolName :: Monad m => CurrencySymbol -> String -> RunT m ()
writeCurrencySymbolName cs name = modifyMockNames $ \ns ->
  ns {mockNameCurrencySymbols = M.insert cs name (mockNameCurrencySymbols ns)}

-- | Assigns human-readable name to a transaction
writeTxName :: Monad m => Tx -> String -> RunT m ()
writeTxName (P.txId . tx'plutus -> ident) name = modifyMockNames $ \ns ->
  ns {mockNameTxns = M.insert ident name (mockNameTxns ns)}

-- | Gets human-readable name of user
readUserName :: MockNames -> PubKeyHash -> Maybe String
readUserName names pkh = M.lookup pkh (mockNameUsers names)

-- | Gets human-readable name of address
readAddressName :: MockNames -> Address -> Maybe String
readAddressName names addr = M.lookup addr (mockNameAddresses names)

-- | Gets human-readable name of user
readAssetClassName :: MockNames -> AssetClass -> Maybe String
readAssetClassName names ac = M.lookup ac (mockNameAssetClasses names)

-- | Gets human-readable name of user
readCurrencySymbolName :: MockNames -> CurrencySymbol -> Maybe String
readCurrencySymbolName names cs = M.lookup cs (mockNameCurrencySymbols names)

-- | Gets human-readable name of transaction
readTxName :: MockNames -> TxId -> Maybe String
readTxName names cs = M.lookup cs (mockNameTxns names)

-- | Reads pretty name for user or script
getPrettyAddress :: (HasAddress user, Monad m) => user -> RunT m String
getPrettyAddress user = do
  names <- gets mockNames
  pure $ fromMaybe (show addr) $ readAddressName names addr <|> (readUserName names =<< toPubKeyHash addr)
  where
    addr = toAddress user

-- | Reads pretty name for currency symbol or just shows the raw one.
getPrettyCurrencySymbol :: Monad m => CurrencySymbol -> RunT m String
getPrettyCurrencySymbol cs = do
  names <- gets mockNames
  pure $ fromMaybe (show cs) $ readCurrencySymbolName names cs

-- | Reads pretty name for currency symbol or just shows the raw one.
getPrettyAssetClass :: Monad m => AssetClass -> RunT m String
getPrettyAssetClass ac = do
  names <- gets mockNames
  pure $ fromMaybe (show ac) $ readAssetClassName names ac

-- | Reads pretty name for currency symbol or just shows the raw one.
getPrettyTxId :: Monad m => TxId -> RunT m String
getPrettyTxId tid = do
  names <- gets mockNames
  pure $ fromMaybe (show tid) $ readTxName names tid

--------------------------------------------------------
-- API

{- | Get pub key hash of the admin user.
 It can be useful to distribute funds to the users.
-}
getMainUser :: Monad m => RunT m PubKeyHash
getMainUser = pure $ userPubKeyHash $ intToUser 0

-- | Run blockchain.
runMock :: Run a -> Mock -> (a, Mock)
runMock act = runIdentity . runMock' act

runMock' :: RunT m a -> Mock -> m (a, Mock) 
runMock' (RunT act) = runStateT act 

-- | Init blockchain state.
initMock :: MockConfig -> Value -> Mock
initMock cfg initVal =
  Mock
    { mockUsers = M.singleton genesisUserId genesisUser
    , mockUtxos = M.singleton genesisTxOutRef genesisTxOut
    , mockDatums = M.empty
    , mockRefScripts = M.empty
    , mockAddresses = M.singleton genesisAddress (S.singleton genesisTxOutRef)
    , mockStake = initStake
    , mockTxs = mempty
    , mockConfig = cfg
    , mockCurrentSlot = Slot 1
    , mockUserStep = 1
    , mockFails = mempty
    , mockInfo = mempty
    , mustFailLog = mempty
    , mockNames = MockNames
                  (M.singleton genesisUserId "Genesis role")
                  (M.singleton genesisAddress "Genesis role")
                  M.empty
                  M.empty
                  M.empty
    }
  where
    genesisUserId = userPubKeyHash genesisUser
    genesisUser = intToUser 0
    genesisAddress = pubKeyHashAddress genesisUserId

    genesisTxOutRef = TxOutRef genesisTxId 0
    genesisTxOut = TxOut (pubKeyHashAddress genesisUserId) initVal NoOutputDatum Nothing

    initStake = Stake
      { stake'pools      = M.singleton genesisPoolId (Pool { pool'stakes = [genesisStakingCred]})
      , stake'poolIds    = V.singleton genesisPoolId
      , stake'stakes     = M.singleton genesisStakingCred 0
      , stake'nextReward = 0
      }

    genesisPoolId = PoolId genesisUserId
    genesisStakingCred = keyToStaking genesisUserId

-- Hash for genesis transaction
dummyHash :: Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody
dummyHash = Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

-- | genesis transaction ID
genesisTxId :: TxId
genesisTxId = fromTxId . Ledger.TxId $ Ledger.unsafeMakeSafeHash dummyHash

-- | Get public key hash for a user
userPubKeyHash :: User -> PubKeyHash
userPubKeyHash (User (C.KeyPair vk _sk)) =
  case C.hashKey vk of
    C.KeyHash h -> PubKeyHash $ toBuiltin $ C.hashToBytes h

-- | Create User out of integer
intToUser :: Integer -> User
intToUser n = User $ C.KeyPair vk sk
  where
    sk = C.genKeyDSIGN $ mkSeedFromInteger $ RawSeed n
    vk = C.VKey $ C.deriveVerKeyDSIGN sk

getUserSignKey :: Monad m => PubKeyHash -> RunT m (Maybe (C.KeyPair 'C.Witness C.StandardCrypto))
getUserSignKey pkh =
  fmap userSignKey . M.lookup pkh <$> gets mockUsers

-- | Sign TX for the user.
signTx :: Monad m => PubKeyHash -> Tx -> RunT m Tx
signTx pkh = updatePlutusTx $ \tx -> do
  mKeys <- getUserSignKey pkh
  case mKeys of
    Just keys -> pure $ tx { P.txSignatures = M.insert pkh keys $ P.txSignatures tx}
    Nothing -> do
      logFail (NoUser pkh)
      pure tx

-- | Return list of failures
getFails :: Monad m => RunT m (Log FailReason)
getFails = gets mockFails

-- | Logs failure and returns it.
pureFail :: Monad m => FailReason -> RunT m Result
pureFail res = do
  logFail res
  pure $ Fail res

-- | Log failure.
logFail :: Monad m => FailReason -> RunT m ()
logFail res = do
  curTime <- gets mockCurrentSlot
  modify' $ \s -> s {mockFails = appendLog curTime res (mockFails s) }

-- | Log generic error.
logError :: Monad m => String -> RunT m ()
logError = logFail . GenericFail

logInfo :: Monad m => String -> RunT m ()
logInfo msg = do
  slot <- gets mockCurrentSlot
  modify' $ \s -> s { mockInfo = appendLog slot msg (mockInfo s) }

-- | Igonres log of TXs and info messages during execution (but not errors)
noLog :: Monad m => RunT m a -> RunT m a
noLog act = do
  txLog <- gets mockTxs
  infoLog <- gets mockInfo
  res <- act
  modify' $ \st -> st { mockTxs = txLog, mockInfo = infoLog }
  pure res

-- | Igonres log of TXs during execution
noLogTx :: Monad m => RunT m a -> RunT m a
noLogTx act = do
  txLog <- gets mockTxs
  res <- act
  modify' $ \st -> st { mockTxs = txLog }
  pure res

-- | Igonres log of info level messages during execution
noLogInfo :: Monad m => RunT m a -> RunT m a
noLogInfo act = do
  infoLog <- gets mockInfo
  res <- act
  modify' $ \st -> st { mockInfo = infoLog }
  pure res

-- | Send block of TXs to blockchain.
sendBlock :: Monad m => [Tx] -> RunT m (Either FailReason [Stat])
sendBlock txs = do
  res <- sequence <$> mapM (sendSingleTx . processMints) txs
  when (isRight res) bumpSlot
  pure res

-- | Sends block with single TX to blockchai
sendTx :: Monad m => Tx -> RunT m (Either FailReason Stat)
sendTx tx = do
  res <- sendSingleTx (processMints tx)
  when (isRight res) bumpSlot
  pure res

{- | Send single TX to blockchain. It logs failure if TX is invalid
 and produces performance stats if TX was ok.
-}
sendSingleTx :: Monad m => Tx -> RunT m (Either FailReason Stat)
sendSingleTx tx = do
  genParams <- gets (mockConfigProtocol . mockConfig)
  case genParams of
    AlonzoParams params  -> checkSingleTx @Alonzo.Era  params tx
    BabbageParams params -> checkSingleTx @Babbage.Era params tx

-- | Confirms that single TX is valid. Works across several Eras (see @Plutus.Model.Fork.Cardano.Class@)
checkSingleTx ::
  forall era m .
  ( Era era,
    ExtendedUTxO era,
    CBOR.ToCBOR (Core.Tx era),
    HasField "inputs" (Core.TxBody era) (Set (C.TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (C.DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (C.Wdrl (Crypto era)),
    HasField "txdats" (Core.Witnesses era) (C.TxDats era),
    HasField "txrdmrs" (Core.Witnesses era) (C.Redeemers era),
    HasField "_costmdls" (Core.PParams era) Alonzo.CostModels,
    HasField "_maxTxExUnits" (Core.PParams era) Alonzo.ExUnits,
    HasField "_protocolVersion" (Core.PParams era) C.ProtVer,
    Core.Script era ~ Alonzo.Script era,
    C.CLI era,
    C.HashAnnotated (Core.TxBody era) C.EraIndependentTxBody C.StandardCrypto,
    Class.IsCardanoTx era,
    Monad m
  )
  => Core.PParams era -> Tx -> RunT m (Either FailReason Stat)
checkSingleTx params (Tx extra tx) =
  withCheckStaking $
    withCheckRange $
      withTxBody $ \txBody -> do
        let tid = fromTxId $ Ledger.txid (Class.getTxBody txBody)
        withUTxO $ \utxo ->
          withCheckBalance utxo txBody $
            withCheckUnits utxo txBody $ \cost -> do
              let txSize = fromIntegral $ BS.length $ CBOR.serialize' txBody
                  stat = Stat txSize cost
              withCheckTxLimits stat $ do
                applyTx stat tid (Tx extra tx)
                pure $ Right stat
  where
    pkhs = M.keys $ P.txSignatures tx

    withTxBody cont = do
      cfg <- gets mockConfig
      let localScriptMap = P.txScripts tx
      case Class.toCardanoTx localScriptMap (mockConfigNetworkId cfg) params (Tx extra tx) of
        Right txBody -> cont txBody
        Left err -> leftFail $ GenericFail err

    withCheckStaking cont = withCheckWithdraw (withCheckCertificates cont )

    withCheckWithdraw cont = maybe cont leftFail =<< checkWithdraws (extra'withdraws extra )
    withCheckCertificates cont = maybe cont leftFail =<< checkCertificates (extra'certificates extra)

    withCheckRange cont = do
      curSlot <- gets mockCurrentSlot
      if Interval.member curSlot $ P.txValidRange tx
        then cont
        else leftFail $ TxInvalidRange curSlot (P.txValidRange tx)

    checkWithdraws ws = do
      st <- gets mockStake
      go st ws
      where
        go st = \case
          [] -> pure Nothing
          Withdraw{..} : rest ->
            case checkWithdrawStake pkhs withdraw'credential withdraw'amount st of
              Nothing  -> go st rest
              Just err -> pure $ Just $ TxInvalidWithdraw err

    checkCertificates certs = do
      st <- gets mockStake
      go st (certificate'dcert <$> certs)
      where
        go st = \case
          []   -> pure Nothing
          c:cs -> case checkDCert c st of
            Nothing  -> go (reactDCert c st) cs
            Just err -> pure $ Just $ TxInvalidCertificate err

    withUTxO cont = do
      mUtxo <- getUTxO tx
      case mUtxo of
        Just (Right utxo) -> cont utxo
        Just (Left err) -> leftFail $ FailToCardano err
        Nothing -> leftFail FailToReadUtxo

    withCheckBalance utxo txBody cont
      | balance == mempty = cont
      | otherwise         = leftFail NotBalancedTx
      where
        balance = evaluateTransactionBalance params utxo isNewPool (Class.getTxBody txBody)

        -- | TODO: use pool ids info
        -- isNewPool :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> Bool
        isNewPool _kh = True -- StakePoolKeyHash kh `S.notMember` poolids

    withCheckUnits ::
         Ledger.UTxO era
      -> Core.Tx era
      -> (Alonzo.ExUnits -> RunT m (Either FailReason a))
      -> RunT m (Either FailReason a)
    withCheckUnits utxo txBody cont = do
      slotCfg <- gets (mockConfigSlotConfig . mockConfig)
      let cardanoSystemStart = SystemStart $ posixSecondsToUTCTime $ fromInteger $ (`div` 1000) $ getPOSIXTime $ scSlotZeroTime slotCfg
          epochSize = EpochSize 1
          slotLength = slotLengthFromMillisec $ scSlotLength slotCfg
          history = fixedEpochInfo @(Either Text) epochSize slotLength
      evalAlonzo cardanoSystemStart history
      where
        foldErrors = lefts
        foldCost = foldMap snd . rights

        evalAlonzo systemStart history = case
          evaluateTransactionExecutionUnits
            params
            txBody
            utxo
            history
            systemStart
            (toAlonzoCostModels $ getField @"_costmdls" params)
          of
            Left err -> leftFail $ GenericFail $ show err
            Right res ->
              let res' = (\(k, v) -> fmap (k,) v) <$> M.toList res
                  errs = foldErrors res'
                  cost = foldCost res'
              in case errs of
                    [] -> cont cost
                    _ -> leftFail $ GenericFail $ unlines $ fmap show errs

        toAlonzoCostModels :: Alonzo.CostModels
                            -> Array.Array Alonzo.Language Alonzo.CostModel
        toAlonzoCostModels (Alonzo.CostModels costmodels) =
          Array.array
            (minBound, maxBound)
            [ (lang, costmodel)
            | (lang, costmodel) <- Map.toList costmodels ]

    withCheckTxLimits stat cont = do
      maxLimits <- gets (mockConfigLimitStats . mockConfig)
      checkLimits <- gets (mockConfigCheckLimits . mockConfig)
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
  , cmp ExMemError (naturalToInteger  . (\(Alonzo.ExUnits mem _)   -> mem)   . statExecutionUnits)
  , cmp ExStepError (naturalToInteger . (\(Alonzo.ExUnits _ steps) -> steps) . statExecutionUnits)
  ]
  where
    cmp cons getter
      | overflow > 0 = Just $ cons overflow (toPercent (getter maxLimits) overflow)
      | otherwise    = Nothing
      where
        overflow = getter stat - getter maxLimits


-- | Read UTxO relevant to transaction
getUTxO :: (Class.IsCardanoTx era, Monad m) => P.Tx -> RunT m (Maybe (Either String (Ledger.UTxO era)))
getUTxO tx = do
  networkId <- mockConfigNetworkId <$> gets mockConfig
  mOuts <- sequence <$> mapM (getTxOut . Plutus.txInRef) ins
  pure $ fmap (Class.toUtxo localScriptMap networkId . zip ins) mOuts
  where
    ins =
      mconcat
        [ S.toList $ P.txInputs tx
        , S.toList $ P.txCollateral tx
        , S.toList $ P.txReferenceInputs tx
        ]

    localScriptMap = P.txScripts tx

-- | Reads TxOut by its reference.
getTxOut :: Monad m => TxOutRef -> RunT m (Maybe TxOut)
getTxOut ref = do
  mTout <- M.lookup ref <$> gets mockUtxos
  case mTout of
    Just tout -> pure $ Just tout
    Nothing   -> M.lookup ref <$> gets mockRefScripts

-- | Update slot counter by one.
bumpSlot :: Monad m => RunT m ()
bumpSlot = modify' $ \s -> s {mockCurrentSlot = mockCurrentSlot s + 1}

-- | Makes slot counter of blockchain to move forward on given amount.
waitNSlots :: Monad m => Slot -> RunT m ()
waitNSlots n = modify' $ \s -> s {mockCurrentSlot = mockCurrentSlot s + n}

-- | Applies valid TX to modify blockchain.
applyTx :: Monad m => Stat -> TxId -> Tx -> RunT m ()
applyTx stat tid etx@(Tx extra P.Tx {..}) = do
  updateUtxos
  updateRewards
  updateCertificates
  updateFees
  saveTx
  saveDatums
  where
    saveDatums = modify' $ \s -> s {mockDatums = txData <> mockDatums s}

    saveTx = do
      t <- gets mockCurrentSlot
      statPercent <- getStatPercent
      modify' $ \s -> s {mockTxs = appendLog t (TxStat etx t stat statPercent) $ mockTxs s}

    getStatPercent = do
      maxLimits <- gets (mockConfigLimitStats . mockConfig)
      pure $ toStatPercent maxLimits stat

    updateUtxos = do
      removeIns txInputs
      mapM_ insertOut $ zip [0 ..] txOutputs

    removeIns ins = modify $ \s ->
      s
        { mockUtxos = rmIns (mockUtxos s)
        , mockAddresses = fmap (`S.difference` inRefSet) (mockAddresses s)
        }
      where
        inRefSet = S.map Plutus.txInRef ins
        inRefs = M.fromList $ (,()) . Plutus.txInRef <$> S.toList ins
        rmIns a = M.difference a inRefs

    insertOut (ix, out) = do
      insertAddresses
      insertUtxos
      where
        ref = TxOutRef tid ix
        addr = txOutAddress out

        insertAddresses = modify' $ \s -> s {mockAddresses = M.alter (Just . maybe (S.singleton ref) (S.insert ref)) addr $ mockAddresses s}
        insertUtxos
          | isRefScript = modify' $ \s -> s {mockRefScripts = M.singleton ref out <> mockRefScripts s}
          | otherwise   = modify' $ \s -> s {mockUtxos = M.singleton ref out <> mockUtxos s}

        isRefScript = isJust (txOutReferenceScript out)

    updateRewards = mapM_ modifyWithdraw $ extra'withdraws extra
      where
        modifyWithdraw Withdraw{..} = onStake (withdrawStake withdraw'credential)

    updateCertificates = mapM_ (onStake . reactDCert . certificate'dcert) $ extra'certificates extra

    onStake f = modify' $ \st -> st { mockStake = f $ mockStake st }

    updateFees = do
      st <- gets mockStake
      forM_ (rewardStake (getLovelace txFee) st) $ \nextSt -> modify' $ \mock -> mock { mockStake = nextSt }

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: Monad m => Address -> RunT m [TxOutRef]
txOutRefAt addr = txOutRefAtState addr <$> get

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: Address -> Mock -> [TxOutRef]
txOutRefAtState addr st = maybe [] S.toList . M.lookup addr $ mockAddresses st

-- | Get all UTXOs that belong to an address (it does not include reference script UTXOs)
utxoAt :: Monad m => HasAddress user => user -> RunT m [(TxOutRef, TxOut)]
utxoAt addr = utxoAtStateBy mockUtxos addr <$> get

-- | Get all reference script UTXOs that belong to an address
refScriptAt :: Monad m => HasAddress user => user -> RunT m [(TxOutRef, TxOut)]
refScriptAt addr = utxoAtStateBy mockRefScripts addr <$> get

-- | Reads the first UTXO by address
withFirstUtxo :: (HasAddress user, Monad m) => user -> ((TxOutRef, TxOut) -> RunT m ()) -> RunT m ()
withFirstUtxo = withUtxo (const True)

-- | Reads list of UTXOs that belong to address and applies predicate to search for
-- certain UTXO in that list. It proceeds with continuation if UTXO is present
-- and fails with @logError@ if there is no such UTXO.
--
-- Note that it does not search among UTXOs that store scripts (used for reference scripts).
-- It's done for convenience.
withUtxo :: (HasAddress user, Monad m) => ((TxOutRef, TxOut) -> Bool) -> user -> ((TxOutRef, TxOut) -> RunT m ()) -> RunT m ()
withUtxo isUtxo user cont =
  withMayBy readMsg (L.find isUtxo <$> utxoAt user) cont
  where
    readMsg = do
      fmap (\name -> "No UTxO for: " <> name) $ getPrettyAddress user

-- | Reads the first reference script UTXO by address
withFirstRefScript :: (HasAddress user, Monad m) => user -> ((TxOutRef, TxOut) -> RunT m ()) -> RunT m ()
withFirstRefScript = withRefScript (const True)

-- | Reads list of reference script UTXOs that belong to address and applies predicate to search for
-- certain UTXO in that list. It proceeds with continuation if UTXO is present
-- and fails with @logError@ if there is no such UTXO.
--
-- Note that it searches only among UTXOs that store scripts (used for reference scripts).
-- It's done for convenience.
withRefScript :: (HasAddress user, Monad m) => ((TxOutRef, TxOut) -> Bool) -> user -> ((TxOutRef, TxOut) -> RunT m ()) -> RunT m ()
withRefScript isUtxo user cont =
  withMayBy readMsg (L.find isUtxo <$> refScriptAt user) cont
  where
    readMsg = do
      fmap (\name -> "No UTxO for: " <> name) $ getPrettyAddress user

-- | Get all UTXOs that belong to an address
utxoAtStateBy :: HasAddress user => (Mock -> Map TxOutRef TxOut) -> user -> Mock -> [(TxOutRef, TxOut)]
utxoAtStateBy extract (toAddress -> addr) st =
  mapMaybe (\r -> (r,) <$> M.lookup r (extract st)) refs
  where
    refs = txOutRefAtState addr st

-- | Reads both hash and inline datums
datumAt :: (FromData a, Monad m) => TxOutRef -> RunT m (Maybe a)
datumAt ref = do
  mdat <- getHashDatum ref
  case mdat of
    Just dat -> pure (Just dat)
    Nothing  -> fmap (getInlineDatum =<< ) $ getTxOut ref

-- | Reads datum with continuation
withDatum :: (FromData a, Monad m) => TxOutRef -> (a -> RunT m ()) -> RunT m ()
withDatum ref cont = withMay err (datumAt ref) cont
  where
    err = "No datum for TxOutRef: "<> show ref

-- | Continuation based queries.
withMay :: Monad m => String -> RunT m (Maybe a) -> (a -> RunT m ()) -> RunT m ()
withMay msg act cont = do
  mRes <- act
  case mRes of
    Just res -> cont res
    Nothing  -> logError msg

-- | Continuation based queries with effectful error messages.
-- It can be useful to read human readable names for addresses, TXs, currency symbols etc.
withMayBy :: Monad m => RunT m String -> RunT m (Maybe a) -> (a -> RunT m ()) -> RunT m ()
withMayBy msg act cont = do
  mRes <- act
  case mRes of
    Just res -> cont res
    Nothing  -> logError =<< msg

-- | Reads typed datum from blockchain that belongs to UTXO (by reference) by Hash.
getHashDatum :: (FromData a, Monad m) => TxOutRef -> RunT m (Maybe a)
getHashDatum ref = do
  dhs <- gets mockDatums
  mDh <- (txOutDatumHash =<<) <$> getTxOut ref
  pure $ fromBuiltinData . getDatum =<< (`M.lookup` dhs) =<< mDh

-- | Reads inlined datum from @TxOut@
getInlineDatum :: FromData dat => TxOut -> Maybe dat
getInlineDatum tout =
  case txOutDatum tout of
    OutputDatum dat -> fromBuiltinData (getDatum dat)
    _               -> Nothing

-- | Reads datum hash for @TxOut@
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash tout =
  case txOutDatum tout of
    OutputDatumHash dh -> Just dh
    _                  -> Nothing

-- | Reads current reward amount for a staking credential
rewardAt :: (HasStakingCredential cred, Monad m) => cred -> RunT m Integer
rewardAt cred = gets (maybe 0 id . lookupReward (toStakingCredential cred) . mockStake)

-- | Returns all stakes delegatged to a pool
stakesAt :: Monad m => PoolId -> RunT m [StakingCredential]
stakesAt (PoolId poolKey) = gets (lookupStakes (PoolId poolKey) . mockStake)

-- | Checks that pool is registered
hasPool :: Monad m => PoolId -> RunT m Bool
hasPool (PoolId pkh) = gets (M.member (PoolId pkh) . stake'pools. mockStake)

-- | Checks that staking credential is registered
hasStake :: (HasStakingCredential a, Monad m) => a -> RunT m Bool
hasStake key = gets (M.member (toStakingCredential key) . stake'stakes. mockStake)

-- | Read pool ids registered on ledger.
getPools :: Monad m => RunT m [PoolId]
getPools = gets (V.toList . stake'poolIds . mockStake)

----------------------------------------------------------------
-- logs

-- | Reads the log.
getLog :: Mock -> Log MockEvent
getLog Mock{..} =
  mconcat
    [ MockInfo <$> mockInfo
    , MockMustFailLog <$> mustFailLog
    , uncurry MockTx . (\tx@(txStatId -> ident) -> (txName ident, tx)) <$> mockTxs
    , MockFail <$> mockFails
    ]
  where
    txName = readTxName mockNames

----------------------------------------------------------------------
-- seed utilities

newtype RawSeed = RawSeed Integer
   deriving newtype (Eq, Show, CBOR.ToCBOR)

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromInteger ::
  RawSeed ->
  C.Seed
mkSeedFromInteger stuff =
  C.mkSeedFromBytes . Crypto.hashToBytes $ Crypto.hashWithSerialiser @Crypto.Blake2b_256 CBOR.toCBOR stuff

