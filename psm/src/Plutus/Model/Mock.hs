{- | Simple test model for plutus scripts.

 We can create blockchain with main user that holds all the value
 and can distribute it to test users.

 The blockchain update happens according to the Cardano node rules
 and is executed as simple state update. We can query all blockchain stats
 for the users.

 Also it estimates execution of the TXs according to cardano model.
-}
module Plutus.Model.Mock (
  -- * Address helpers
  HasAddress (..),
  HasStakingCredential (..),
  AppendStaking (..),
  appendStakingCredential,
  appendStakingPubKey,
  appendStakingScript,

  -- * Mock blockchain model
  Mock (..),
  mockRefScripts,
  MockConfig (..),
  CheckLimits (..),
  MockNames (..),
  User (..),
  TxStat (..),
  txStatId,
  PoolId (..),
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
  Run (..),
  runMock,
  initMock,
  Percent (..),
  toPercent,
  StatPercent (..),
  PercentExecutionUnits (..),
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
  defaultBabbageV1,
  defaultBabbageV2,
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
  Log (..),
  appendLog,
  nullLog,
  fromLog,
  fromGroupLog,
  MockEvent (..),
  silentLog,
  failLog,
  filterSlot,
  getLog,
  getFails,
  MustFailLog (..),

  -- * internal
  intToUser,
  userPubKeyHash,
) where

import Control.Applicative (Alternative (..))
import GHC.Records
import Prelude

import Control.Monad.Identity
import Data.ByteString qualified as BS
import Data.Either
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

import Data.List qualified as L
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V

import Cardano.Crypto.DSIGN.Class qualified as C
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.Seed qualified as C
import Cardano.Ledger.Alonzo.Tx qualified as C
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Crypto qualified as C
import Cardano.Ledger.Shelley.API.Types qualified as C
import Cardano.Simple.Ledger.Tx qualified as P
import Cardano.Simple.Ledger.Tx qualified as Plutus
import Control.Monad.State.Strict
import GHC.Natural
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, toPubKeyHash)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value (AssetClass, assetClass)
import PlutusLedgerApi.V2 hiding (Map)

import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Hashes as Ledger (EraIndependentTxBody)
import Cardano.Ledger.SafeHash qualified as Ledger (unsafeMakeSafeHash)
import Cardano.Simple.Ledger.Slot
import Cardano.Simple.TxExtra
import Plutus.Model.Mock.ProtocolParameters
import Plutus.Model.Stake

import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Block qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Ledger.Shelley.API.Wallet qualified as C
import Cardano.Ledger.TxIn qualified as Ledger
import Cardano.Simple.Cardano.Alonzo ()
import Cardano.Simple.Cardano.Alonzo qualified as Alonzo
import Cardano.Simple.Cardano.Babbage ()
import Cardano.Simple.Cardano.Babbage qualified as Babbage
import Cardano.Simple.Cardano.Class qualified as Class
import Cardano.Simple.Cardano.Common (fromCardanoValue, fromTxId)
import Cardano.Simple.Eval (evaluateScriptsInTx, txBalance)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (catchError, throwError), liftEither, runExceptT)
import Plutus.Model.Ada (Ada (..))
import Plutus.Model.Mock.Address
import Plutus.Model.Mock.FailReason
import Plutus.Model.Mock.Log
import Plutus.Model.Mock.MockConfig
import Plutus.Model.Mock.Stat

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
  { mockUsers :: !(Map PubKeyHash User)
  , mockAddresses :: !(Map Address (Set TxOutRef))
  , mockUtxos :: !(Map TxOutRef TxOut)
  -- ^ Since 0.4, reference script UTxOs are also included.
  , mockDatums :: !(Map DatumHash Datum)
  , mockStake :: !Stake
  , mockTxs :: !(Log TxStat)
  , mockConfig :: !MockConfig
  , mockCurrentSlot :: !Slot
  , mockUserStep :: !Integer
  , mockFails :: !(Log FailReason)
  , mockInfo :: !(Log String)
  , mustFailLog :: !(Log MustFailLog)
  , mockNames :: !MockNames
  -- ^ human readable names. Idea is to substitute for them
  -- in pretty printers for error logs, user names, script names.
  }

{- | All UTxOs containing reference scripts.

Since 0.4 this has been a function rather than a field.
-}
mockRefScripts :: Mock -> Map TxOutRef TxOut
mockRefScripts = Map.filter (isJust . txOutReferenceScript) . mockUtxos

-- | Result of the execution.
data Result = Ok | Fail FailReason
  deriving (Show)

-- | Result is ok.
isOkResult :: Result -> Bool
isOkResult = \case
  Ok -> True
  _ -> False

-- | State monad wrapper to run blockchain.
newtype Run a = Run (State Mock a)
  deriving newtype (Functor, Applicative, Monad, MonadState Mock)

{- | Dummy instance to be able to use partial pattern matching
 in do-notation
-}
instance MonadFail Run where
  fail err = error $ "Failed to recover: " <> err

-- | Human readable names for pretty printing.
data MockNames = MockNames
  { mockNameUsers :: !(Map PubKeyHash String)
  , mockNameAddresses :: !(Map Address String)
  , mockNameAssetClasses :: !(Map AssetClass String)
  , mockNameCurrencySymbols :: !(Map CurrencySymbol String)
  , mockNameTxns :: !(Map TxId String)
  }

-- | Modifies the mappings to human-readable names
modifyMockNames :: (MockNames -> MockNames) -> Run ()
modifyMockNames f = modify' $ \s -> s {mockNames = f (mockNames s)}

-- | Assigns human-readable name to user
writeUserName :: PubKeyHash -> String -> Run ()
writeUserName pkh name = do
  modifyMockNames $ \ns ->
    ns {mockNameUsers = M.insert pkh name (mockNameUsers ns)}
  writeAddressName (pubKeyHashAddress pkh) name

-- | Assigns human-readable name to address
writeAddressName :: Address -> String -> Run ()
writeAddressName addr name = modifyMockNames $ \ns ->
  ns {mockNameAddresses = M.insert addr name (mockNameAddresses ns)}

-- | Assigns human-readable name to asset class
writeAssetClassName :: AssetClass -> String -> Run ()
writeAssetClassName ac name = modifyMockNames $ \ns ->
  ns {mockNameAssetClasses = M.insert ac name (mockNameAssetClasses ns)}

-- | Assigns human-readable name to currency symbol
writeCurrencySymbolName :: CurrencySymbol -> String -> Run ()
writeCurrencySymbolName cs name = modifyMockNames $ \ns ->
  ns {mockNameCurrencySymbols = M.insert cs name (mockNameCurrencySymbols ns)}

-- | Assigns human-readable name to a transaction
writeTxName :: Tx -> String -> Run ()
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
getPrettyAddress :: HasAddress user => user -> Run String
getPrettyAddress user = do
  names <- gets mockNames
  pure $ fromMaybe (show addr) $ readAddressName names addr <|> (readUserName names =<< toPubKeyHash addr)
  where
    addr = toAddress user

-- | Reads pretty name for currency symbol or just shows the raw one.
getPrettyCurrencySymbol :: CurrencySymbol -> Run String
getPrettyCurrencySymbol cs = do
  names <- gets mockNames
  pure $ fromMaybe (show cs) $ readCurrencySymbolName names cs

-- | Reads pretty name for currency symbol or just shows the raw one.
getPrettyAssetClass :: AssetClass -> Run String
getPrettyAssetClass ac = do
  names <- gets mockNames
  pure $ fromMaybe (show ac) $ readAssetClassName names ac

-- | Reads pretty name for currency symbol or just shows the raw one.
getPrettyTxId :: TxId -> Run String
getPrettyTxId tid = do
  names <- gets mockNames
  pure $ fromMaybe (show tid) $ readTxName names tid

--------------------------------------------------------
-- API

{- | Get pub key hash of the admin user.
 It can be useful to distribute funds to the users.
-}
getMainUser :: Run PubKeyHash
getMainUser = pure $ userPubKeyHash $ intToUser 0

-- | Run blockchain.
runMock :: Run a -> Mock -> (a, Mock)
runMock (Run act) = runState act

-- | Init blockchain state.
initMock :: MockConfig -> Value -> Mock
initMock cfg initVal =
  Mock
    { mockUsers = M.singleton genesisUserId genesisUser
    , mockUtxos = M.singleton genesisTxOutRef genesisTxOut
    , mockDatums = M.empty
    , mockAddresses = M.singleton genesisAddress (S.singleton genesisTxOutRef)
    , mockStake = initStake
    , mockTxs = mempty
    , mockConfig = cfg
    , mockCurrentSlot = Slot 1
    , mockUserStep = 1
    , mockFails = mempty
    , mockInfo = mempty
    , mustFailLog = mempty
    , mockNames =
        MockNames
          { mockNameUsers = M.singleton genesisUserId "Genesis role"
          , mockNameAddresses = M.singleton genesisAddress "Genesis role"
          , mockNameAssetClasses = M.singleton (assetClass adaSymbol adaToken) "ADA"
          , mockNameCurrencySymbols = M.singleton adaSymbol "ADA"
          , mockNameTxns = M.empty
          }
    }
  where
    genesisUserId = userPubKeyHash genesisUser
    genesisUser = intToUser 0
    genesisAddress = pubKeyHashAddress genesisUserId

    genesisTxOutRef = TxOutRef genesisTxId 0
    genesisTxOut = TxOut (pubKeyHashAddress genesisUserId) initVal NoOutputDatum Nothing

    initStake =
      Stake
        { stake'pools = M.singleton genesisPoolId (Pool {pool'stakes = [genesisStakingCred]})
        , stake'poolIds = V.singleton genesisPoolId
        , stake'stakes = M.singleton genesisStakingCred 0
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

getUserSignKey :: PubKeyHash -> Run (Maybe (C.KeyPair 'C.Witness C.StandardCrypto))
getUserSignKey pkh =
  fmap userSignKey . M.lookup pkh <$> gets mockUsers

-- | Sign TX for the user.
signTx :: PubKeyHash -> Tx -> Run Tx
signTx pkh = updatePlutusTx $ \tx -> do
  mKeys <- getUserSignKey pkh
  case mKeys of
    Just keys -> pure $ tx {P.txSignatures = M.insert pkh keys $ P.txSignatures tx}
    Nothing -> do
      logFail (NoUser pkh)
      pure tx

-- | Return list of failures
getFails :: Run (Log FailReason)
getFails = gets mockFails

-- | Logs failure and returns it.
pureFail :: FailReason -> Run Result
pureFail res = do
  logFail res
  pure $ Fail res

-- | Log failure.
logFail :: FailReason -> Run ()
logFail res = do
  curTime <- gets mockCurrentSlot
  modify' $ \s -> s {mockFails = appendLog curTime res (mockFails s)}

-- | Log generic error.
logError :: String -> Run ()
logError = logFail . GenericFail

logInfo :: String -> Run ()
logInfo msg = do
  slot <- gets mockCurrentSlot
  modify' $ \s -> s {mockInfo = appendLog slot msg (mockInfo s)}

-- | Ignore log of TXs and info messages during execution (but not errors)
noLog :: Run a -> Run a
noLog act = do
  txLog <- gets mockTxs
  infoLog <- gets mockInfo
  res <- act
  modify' $ \st -> st {mockTxs = txLog, mockInfo = infoLog}
  pure res

-- | Ignore log of TXs during execution
noLogTx :: Run a -> Run a
noLogTx act = do
  txLog <- gets mockTxs
  res <- act
  modify' $ \st -> st {mockTxs = txLog}
  pure res

-- | Ignore log of info level messages during execution
noLogInfo :: Run a -> Run a
noLogInfo act = do
  infoLog <- gets mockInfo
  res <- act
  modify' $ \st -> st {mockInfo = infoLog}
  pure res

-- | Send block of TXs to blockchain.
sendBlock :: [Tx] -> Run (Either FailReason [Stat])
sendBlock txs = do
  res <- sequence <$> mapM sendSingleTx txs
  when (isRight res) bumpSlot
  pure res

-- | Sends block with single TX to blockchain
sendTx :: Tx -> Run (Either FailReason Stat)
sendTx tx = do
  res <- sendSingleTx tx
  when (isRight res) bumpSlot
  pure res

{- | Send single TX to blockchain. It logs failure if TX is invalid
 and produces performance stats if TX was ok.
-}
sendSingleTx :: Tx -> Run (Either FailReason Stat)
sendSingleTx preTx@(Tx extra _) =
  runValidate $ do
    tx <- liftEither (processMints preTx)
    genParams <- gets (mockConfigProtocol . mockConfig)
    case genParams of
      AlonzoParams params -> checkSingleTx @Alonzo.Era params extra tx
      BabbageParams params -> checkSingleTx @Babbage.Era params extra tx

-- | Confirms that single TX is valid. Works across several Eras (see @Cardano.Simple.Cardano.Class@)
checkSingleTx ::
  forall era.
  ( ExtendedUTxO era
  , HasField "_costmdls" (Core.PParams era) Alonzo.CostModels
  , HasField "_maxTxExUnits" (Core.PParams era) Alonzo.ExUnits
  , HasField "_protocolVersion" (Core.PParams era) C.ProtVer
  , Core.Script era ~ Alonzo.AlonzoScript era
  , Class.IsCardanoTx era
  , Core.Value era ~ Mary.MaryValue C.StandardCrypto
  , C.CLI era
  , C.AlonzoEraTx era
  ) =>
  Core.PParams era ->
  Extra ->
  P.Tx ->
  Validate Stat
checkSingleTx params extra tx = do
  checkStaking
  checkRange
  txBody <- getTxBody
  let tid = fromTxId $ Ledger.txid (Class.getTxBody txBody)
  checkBalance
  cost <- evalScripts
  let txSize = fromIntegral $ BS.length $ CBOR.serialize' txBody
      stat = Stat txSize cost
  checkTxLimits stat
  Validate . lift $ applyTx stat tid extra tx
  pure stat
  where
    pkhs = M.keys $ P.txSignatures tx

    getTxBody :: Validate (Core.Tx era)
    getTxBody = do
      cfg <- gets mockConfig
      orFailValidate GenericFail $
        Class.toCardanoTx
          (mockConfigNetworkId cfg)
          params
          extra
          tx

    checkStaking :: Validate ()
    checkStaking = do
      checkWithdraws (extra'withdraws extra)
      checkCertificates (extra'certificates extra)

    checkRange :: Validate ()
    checkRange = do
      curSlot <- gets mockCurrentSlot
      unless
        (Interval.member curSlot $ P.txValidRange tx)
        (throwError $ TxInvalidRange curSlot (P.txValidRange tx))

    checkWithdraws :: [Withdraw] -> Validate ()
    checkWithdraws ws = do
      st <- gets mockStake
      go st ws
      where
        go st = \case
          [] -> pure ()
          Withdraw {..} : rest ->
            case checkWithdrawStake pkhs withdraw'credential withdraw'amount st of
              Nothing -> go st rest
              Just err -> throwError (TxInvalidWithdraw err)

    checkCertificates :: [Certificate] -> Validate ()
    checkCertificates certs = do
      st <- gets mockStake
      go st (certificate'dcert <$> certs)
      where
        go :: Stake -> [DCert] -> Validate ()
        go st = \case
          [] -> pure ()
          c : cs -> case checkDCert c st of
            Nothing -> go (reactDCert c st) cs
            Just err -> throwError (TxInvalidCertificate err)

    checkBalance :: Validate ()
    checkBalance = do
      utxos <- gets mockUtxos
      network <- gets $ mockConfigNetworkId . mockConfig
      case txBalance @era utxos params network tx extra of
        Left err -> throwError $ FailToCardano err
        Right bal ->
          when
            (bal /= mempty)
            (throwError $ NotBalancedTx $ fromCardanoValue bal)

    evalScripts :: Validate Alonzo.ExUnits
    evalScripts = do
      utxos <- gets mockUtxos
      network <- gets $ mockConfigNetworkId . mockConfig
      slotCfg <- gets (mockConfigSlotConfig . mockConfig)
      case evaluateScriptsInTx @era utxos params network tx extra slotCfg of
        Right units -> pure units
        Left (Left err) -> throwError $ GenericFail err
        Left (Right err) -> throwError $ FailToCardano $ show err

    checkTxLimits :: Stat -> Validate ()
    checkTxLimits stat = do
      maxLimits <- gets (mockConfigLimitStats . mockConfig)
      checkLimits <- gets (mockConfigCheckLimits . mockConfig)
      let errs = compareLimits maxLimits stat
          statPercent = toStatPercent maxLimits stat
      unless
        (null errs)
        ( case checkLimits of
            IgnoreLimits -> pure ()
            WarnLimits -> throwError (TxLimitError errs statPercent)
            ErrorLimits -> throwError (TxLimitError errs statPercent)
        )

newtype Validate a = Validate {unValidate :: ExceptT FailReason Run a}
  deriving newtype (Functor, Applicative, Monad, MonadState Mock)

runValidate :: Validate a -> Run (Either FailReason a)
runValidate = runExceptT . unValidate

orFailValidate :: (e -> FailReason) -> Either e a -> Validate a
orFailValidate err = either (throwError . err) pure

instance MonadError FailReason Validate where
  throwError err = Validate $ lift (logFail err) >> throwError err
  catchError (Validate a) cont = Validate $ catchError a (unValidate . cont)

compareLimits :: Stat -> Stat -> [LimitOverflow]
compareLimits maxLimits stat =
  catMaybes
    [ cmp TxSizeError statSize
    , cmp ExMemError (naturalToInteger . (\(Alonzo.ExUnits mem _) -> mem) . statExecutionUnits)
    , cmp ExStepError (naturalToInteger . (\(Alonzo.ExUnits _ steps) -> steps) . statExecutionUnits)
    ]
  where
    cmp cons getter
      | overflow > 0 = Just $ cons overflow (toPercent (getter maxLimits) overflow)
      | otherwise = Nothing
      where
        overflow = getter stat - getter maxLimits

-- | Reads TxOut by its reference.
getTxOut :: TxOutRef -> Run (Maybe TxOut)
getTxOut ref = gets (M.lookup ref . mockUtxos)

-- | Update slot counter by one.
bumpSlot :: Run ()
bumpSlot = modify' $ \s -> s {mockCurrentSlot = mockCurrentSlot s + 1}

-- | Makes slot counter of blockchain to move forward on given amount.
waitNSlots :: Slot -> Run ()
waitNSlots n = modify' $ \s -> s {mockCurrentSlot = mockCurrentSlot s + n}

-- | Applies valid TX to modify blockchain.
applyTx :: Stat -> TxId -> Extra -> P.Tx -> Run ()
applyTx stat tid extra tx@P.Tx {..} = do
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
      modify' $ \s -> s {mockTxs = appendLog t (TxStat tx t stat statPercent) $ mockTxs s}

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
        insertUtxos = modify' $ \s -> s {mockUtxos = M.singleton ref out <> mockUtxos s}

    updateRewards = mapM_ modifyWithdraw $ extra'withdraws extra
      where
        modifyWithdraw Withdraw {..} = onStake (withdrawStake withdraw'credential)

    updateCertificates = mapM_ (onStake . reactDCert . certificate'dcert) $ extra'certificates extra

    onStake f = modify' $ \st -> st {mockStake = f $ mockStake st}

    updateFees = do
      st <- gets mockStake
      forM_ (rewardStake (getLovelace txFee) st) $ \nextSt -> modify' $ \mock -> mock {mockStake = nextSt}

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: Address -> Run [TxOutRef]
txOutRefAt addr = txOutRefAtState addr <$> get

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: Address -> Mock -> [TxOutRef]
txOutRefAtState addr st = foldMap S.toList . M.lookup addr $ mockAddresses st

{- | Get all UTXOs that belong to an address.

Since 0.4 this includes UTxOs that have reference scripts.
-}
utxoAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
utxoAt addr = gets (utxoAtStateBy mockUtxos addr)

-- | Get all reference script UTXOs that belong to an address
refScriptAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
refScriptAt addr = gets (utxoAtStateBy mockRefScripts addr)

-- | Reads the first UTXO by address
withFirstUtxo :: HasAddress user => user -> ((TxOutRef, TxOut) -> Run ()) -> Run ()
withFirstUtxo = withUtxo (const True)

{- | Reads list of UTXOs that belong to address and applies predicate to search for
 certain UTXO in that list. It proceeds with continuation if UTXO is present
 and fails with @logError@ if there is no such UTXO.

Since 0.4 this includes UTxOs that have reference scripts.
-}
withUtxo :: HasAddress user => ((TxOutRef, TxOut) -> Bool) -> user -> ((TxOutRef, TxOut) -> Run ()) -> Run ()
withUtxo isUtxo user = withMayBy readMsg (L.find isUtxo <$> utxoAt user)
  where
    readMsg = do
      ("No UTxO for: " <>) <$> getPrettyAddress user

-- | Reads the first reference script UTXO by address
withFirstRefScript :: HasAddress user => user -> ((TxOutRef, TxOut) -> Run ()) -> Run ()
withFirstRefScript = withRefScript (const True)

{- | Reads list of reference script UTXOs that belong to address and applies predicate to search for
 certain UTXO in that list. It proceeds with continuation if UTXO is present
 and fails with @logError@ if there is no such UTXO.

 Note that it searches only among UTXOs that store scripts (used for reference scripts).
 It's done for convenience.
-}
withRefScript :: HasAddress user => ((TxOutRef, TxOut) -> Bool) -> user -> ((TxOutRef, TxOut) -> Run ()) -> Run ()
withRefScript isUtxo user =
  withMayBy readMsg (L.find isUtxo <$> refScriptAt user)
  where
    readMsg = do
      ("No UTxO for: " <>) <$> getPrettyAddress user

-- | Get all UTXOs that belong to an address
utxoAtStateBy :: HasAddress user => (Mock -> Map TxOutRef TxOut) -> user -> Mock -> [(TxOutRef, TxOut)]
utxoAtStateBy extract (toAddress -> addr) st =
  mapMaybe (\r -> (r,) <$> M.lookup r (extract st)) refs
  where
    refs = txOutRefAtState addr st

-- | Reads both hash and inline datums
datumAt :: FromData a => TxOutRef -> Run (Maybe a)
datumAt ref = do
  mdat <- getHashDatum ref
  case mdat of
    Just dat -> pure (Just dat)
    Nothing -> (getInlineDatum =<<) <$> getTxOut ref

-- | Reads datum with continuation
withDatum :: FromData a => TxOutRef -> (a -> Run ()) -> Run ()
withDatum ref = withMay err (datumAt ref)
  where
    err = "No datum for TxOutRef: " <> show ref

-- | Continuation based queries.
withMay :: String -> Run (Maybe a) -> (a -> Run ()) -> Run ()
withMay msg act cont = do
  mRes <- act
  case mRes of
    Just res -> cont res
    Nothing -> logError msg

{- | Continuation based queries with effective error messages.
 It can be useful to read human readable names for addresses, TXs, currency symbols etc.
-}
withMayBy :: Run String -> Run (Maybe a) -> (a -> Run ()) -> Run ()
withMayBy msg act cont = do
  mRes <- act
  case mRes of
    Just res -> cont res
    Nothing -> logError =<< msg

-- | Reads typed datum from blockchain that belongs to UTXO (by reference) by Hash.
getHashDatum :: FromData a => TxOutRef -> Run (Maybe a)
getHashDatum ref = do
  dhs <- gets mockDatums
  mDh <- (txOutDatumHash =<<) <$> getTxOut ref
  pure $ fromBuiltinData . getDatum =<< (`M.lookup` dhs) =<< mDh

-- | Reads inlined datum from @TxOut@
getInlineDatum :: FromData dat => TxOut -> Maybe dat
getInlineDatum tout =
  case txOutDatum tout of
    OutputDatum dat -> fromBuiltinData (getDatum dat)
    _ -> Nothing

-- | Reads datum hash for @TxOut@
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash tout =
  case txOutDatum tout of
    OutputDatumHash dh -> Just dh
    _ -> Nothing

-- | Reads current reward amount for a staking credential
rewardAt :: HasStakingCredential cred => cred -> Run Integer
rewardAt cred = gets (fromMaybe 0 . lookupReward (toStakingCredential cred) . mockStake)

-- | Returns all stakes delegated to a pool
stakesAt :: PoolId -> Run [StakingCredential]
stakesAt (PoolId poolKey) = gets (lookupStakes (PoolId poolKey) . mockStake)

-- | Checks that pool is registered
hasPool :: PoolId -> Run Bool
hasPool (PoolId pkh) = gets (M.member (PoolId pkh) . stake'pools . mockStake)

-- | Checks that staking credential is registered
hasStake :: HasStakingCredential a => a -> Run Bool
hasStake key = gets (M.member (toStakingCredential key) . stake'stakes . mockStake)

-- | Read pool ids registered on ledger.
getPools :: Run [PoolId]
getPools = gets (V.toList . stake'poolIds . mockStake)

----------------------------------------------------------------
-- logs

-- | Reads the log.
getLog :: Mock -> Log MockEvent
getLog Mock {..} =
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

{- | Construct a seed from a bunch of Word64s

   We multiply these words by some extra stuff to make sure they contain
   enough bits for our seed.
-}
mkSeedFromInteger ::
  RawSeed ->
  C.Seed
mkSeedFromInteger stuff =
  C.mkSeedFromBytes . Crypto.hashToBytes $ Crypto.hashWithSerialiser @Crypto.Blake2b_256 CBOR.toCBOR stuff
