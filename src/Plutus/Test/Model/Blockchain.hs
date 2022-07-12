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
  HasStakingCredential(..),
  AppendStaking(..),
  appendStakingCredential,
  appendStakingPubKey,
  appendStakingScript,
  -- * Blockchain model
  Blockchain (..),
  BchConfig (..),
  CheckLimits(..),
  BchNames (..),
  User (..),
  TxStat (..),
  txStatId,
  PoolId(..),
  ExUnits (..),
  Result (..),
  isOkResult,
  FailReason (..),
  LimitOverflow (..),
  modifyBchNames,
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
  noLog,
  noLogTx,
  noLogInfo,
  pureFail,
  txOutRefAt,
  getTxOut,
  utxoAt,
  utxoAtState,
  datumAt,
  rewardAt,
  stakesAt,
  hasPool,
  hasStake,
  getPools,
  waitNSlots,

  -- * Blockchain config
  readBchConfig,
  defaultAlonzo,
  defaultBchConfig,
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
  MustFailLog(..),

  -- * internal
  intToUser,
  userPubKeyHash,
) where

import Prelude

import Control.Monad.Identity
import Data.ByteString qualified as BS
import Data.Either
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Text (Text)

import Data.Vector qualified as V
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cardano.Ledger.Shelley.API.Types qualified as C
import Cardano.Crypto.Seed qualified as C
import Cardano.Crypto.DSIGN.Class qualified as C
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Ledger.Crypto qualified as C
import Cardano.Slotting.EpochInfo.Impl (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), slotLengthFromMillisec)
import Control.Monad.State.Strict
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval ()
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Tx qualified as P
import Plutus.Test.Model.Fork.Ledger.Tx qualified as P
import Plutus.V1.Ledger.Value (AssetClass)
import GHC.Natural

import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Hashes as Ledger (EraIndependentTxBody)
import Cardano.Ledger.SafeHash qualified as Ledger (unsafeMakeSafeHash)
import Plutus.Test.Model.Fork.Ledger.Slot
import Plutus.Test.Model.Fork.Ledger.TimeSlot (SlotConfig (..))
import Plutus.Test.Model.Fork.TxExtra
import Plutus.Test.Model.Stake
import Plutus.Test.Model.Blockchain.ProtocolParameters

import Cardano.Ledger.TxIn qualified as Ledger
import Cardano.Ledger.Alonzo.Tools (evaluateTransactionExecutionUnits)
import Cardano.Ledger.Shelley.API.Wallet (evaluateTransactionBalance)
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Plutus.Test.Model.Fork.Cardano.Alonzo (toAlonzoTx)
import Plutus.Test.Model.Fork.Cardano.Alonzo qualified as Alonzo (Era, fromTxId, toUtxo)
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import Plutus.Test.Model.Fork.Ledger.Ada (Ada(..))
import Plutus.Test.Model.Blockchain.BchConfig
import Plutus.Test.Model.Blockchain.Log
import Plutus.Test.Model.Blockchain.Address
import Plutus.Test.Model.Blockchain.Stat


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
data Blockchain = Blockchain
  { bchUsers        :: !(Map PubKeyHash User)
  , bchAddresses    :: !(Map Address (Set TxOutRef))
  , bchUtxos        :: !(Map TxOutRef TxOut)
  , bchDatums       :: !(Map DatumHash Datum)
  , bchStake        :: !Stake
  , bchTxs          :: !(Log TxStat)
  , bchConfig       :: !BchConfig
  , bchCurrentSlot  :: !Slot
  , bchUserStep     :: !Integer
  , bchFails        :: !(Log FailReason)
  , bchInfo         :: !(Log String)
  , mustFailLog     :: !(Log MustFailLog)
  , -- | human readable names. Idea is to substitute for them
    -- in pretty printers for error logs, user names, script names.
    bchNames :: !BchNames
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
newtype Run a = Run (State Blockchain a)
  deriving newtype (Functor, Applicative, Monad, MonadState Blockchain)

-- | Dummy instance to be able to use partial pattern matching
-- in do-notation
instance MonadFail Run where
   fail err = error $ "Failed to recover: " <> err

-- | Human readable names for pretty printing.
data BchNames = BchNames
  { bchNameUsers :: !(Map PubKeyHash String)
  , bchNameAddresses :: !(Map Address String)
  , bchNameAssetClasses :: !(Map AssetClass String)
  , bchNameCurrencySymbols :: !(Map CurrencySymbol String)
  , bchNameTxns :: !(Map TxId String)
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

-- | Assigns human-readable name to a transaction
writeTxName :: Tx -> String -> Run ()
writeTxName (P.txId . tx'plutus -> ident) name = modifyBchNames $ \ns ->
  ns {bchNameTxns = M.insert ident name (bchNameTxns ns)}

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

-- | Gets human-readable name of transaction
readTxName :: BchNames -> TxId -> Maybe String
readTxName names cs = M.lookup cs (bchNameTxns names)

--------------------------------------------------------
-- API

{- | Get pub key hash of the admin user.
 It can be useful to distribute funds to the users.
-}
getMainUser :: Run PubKeyHash
getMainUser = pure $ userPubKeyHash $ intToUser 0

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
    , bchStake = initStake
    , bchTxs = mempty
    , bchConfig = cfg
    , bchCurrentSlot = Slot 1
    , bchUserStep = 1
    , bchFails = mempty
    , bchInfo = mempty
    , mustFailLog = mempty
    , bchNames = BchNames
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
    genesisTxOut = TxOut (pubKeyHashAddress genesisUserId) initVal Nothing

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
genesisTxId = Alonzo.fromTxId . Ledger.TxId $ Ledger.unsafeMakeSafeHash dummyHash

userPubKeyHash :: User -> PubKeyHash
userPubKeyHash (User (C.KeyPair vk _sk)) =
  case C.hashKey vk of
    C.KeyHash h -> PubKeyHash $ toBuiltin $ C.hashToBytes h

intToUser :: Integer -> User
intToUser n = User $ C.KeyPair vk sk
  where
    sk = C.genKeyDSIGN $ mkSeedFromInteger $ RawSeed n
    vk = C.VKey $ C.deriveVerKeyDSIGN sk

getUserSignKey :: PubKeyHash -> Run (Maybe (C.KeyPair 'C.Witness C.StandardCrypto))
getUserSignKey pkh =
  fmap userSignKey . M.lookup pkh <$> gets bchUsers

-- | Sign TX for the user.
signTx :: PubKeyHash -> Tx -> Run Tx
signTx pkh = updatePlutusTx $ \tx -> do
  mKeys <- getUserSignKey pkh
  case mKeys of
    Just keys -> pure $ tx { P.txSignatures = M.insert pkh keys $ P.txSignatures tx}
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

-- | Igonres log of TXs and info messages during execution (but not errors)
noLog :: Run a -> Run a
noLog act = do
  txLog <- gets bchTxs
  infoLog <- gets bchInfo
  res <- act
  modify' $ \st -> st { bchTxs = txLog, bchInfo = infoLog }
  pure res

-- | Igonres log of TXs during execution
noLogTx :: Run a -> Run a
noLogTx act = do
  txLog <- gets bchTxs
  res <- act
  modify' $ \st -> st { bchTxs = txLog }
  pure res

-- | Igonres log of info level messages during execution
noLogInfo :: Run a -> Run a
noLogInfo act = do
  infoLog <- gets bchInfo
  res <- act
  modify' $ \st -> st { bchInfo = infoLog }
  pure res

-- | Send block of TXs to blockchain.
sendBlock :: [Tx] -> Run (Either FailReason [Stat])
sendBlock txs = do
  res <- sequence <$> mapM (sendSingleTx . processMints) txs
  when (isRight res) bumpSlot
  pure res

-- | Sends block with single TX to blockchai
sendTx :: Tx -> Run (Either FailReason Stat)
sendTx tx = do
  res <- sendSingleTx (processMints tx)
  when (isRight res) bumpSlot
  pure res

{- | Send single TX to blockchain. It logs failure if TX is invalid
 and produces performance stats if TX was ok.
-}
sendSingleTx :: Tx -> Run (Either FailReason Stat)
sendSingleTx (Tx extra tx) =
  withCheckStaking $
    withCheckRange $
      withTxBody $ \protocol txBody -> do
        let tid = Alonzo.fromTxId $ Ledger.txid (Alonzo.body txBody)
        withUTxO $ \utxo ->
          withCheckBalance protocol utxo txBody $
            withCheckUnits protocol utxo txBody $ \cost -> do
              let txSize = fromIntegral $ BS.length $ CBOR.serialize' txBody
                  stat = Stat txSize cost
              withCheckTxLimits stat $ do
                applyTx stat tid (Tx extra tx)
                pure $ Right stat
  where
    pkhs = M.keys $ P.txSignatures tx

    withCheckStaking cont = withCheckWithdraw (withCheckCertificates cont )

    withCheckWithdraw cont = maybe cont leftFail =<< checkWithdraws (extra'withdraws extra )
    withCheckCertificates cont = maybe cont leftFail =<< checkCertificates (extra'certificates extra)

    checkWithdraws ws = do
      st <- gets bchStake
      go st ws
      where
        go st = \case
          [] -> pure Nothing
          Withdraw{..} : rest ->
            case checkWithdrawStake pkhs withdraw'credential withdraw'amount st of
              Nothing  -> go st rest
              Just err -> pure $ Just $ TxInvalidWithdraw err

    checkCertificates certs = do
      st <- gets bchStake
      go st (certificate'dcert <$> certs)
      where
        go st = \case
          []   -> pure Nothing
          c:cs -> case checkDCert c st of
            Nothing  -> go (reactDCert c st) cs
            Just err -> pure $ Just $ TxInvalidCertificate err

    withCheckRange cont = do
      curSlot <- gets bchCurrentSlot
      if Interval.member curSlot $ P.txValidRange tx
        then cont
        else leftFail $ TxInvalidRange curSlot (P.txValidRange tx)

    withUTxO cont = do
      mUtxo <- getUTxO tx
      case mUtxo of
        Just (Right utxo) -> cont utxo
        Just (Left err) -> leftFail $ FailToCardano err
        Nothing -> leftFail FailToReadUtxo

    withTxBody cont = do
      cfg <- gets bchConfig
      case bchConfigProtocol cfg of
        AlonzoParams params   -> do
          case toAlonzoTx (bchConfigNetworkId cfg) params (Tx extra tx) of
            Right txBody -> cont params txBody
            Left err -> leftFail $ GenericFail err

        BabbageParams _params -> undefined {- TODO -}

    withCheckBalance params utxo txBody cont
      | balanceIsOk = cont
      | otherwise = leftFail NotBalancedTx
      where
        balanceIsOk = alonzoBalance == mempty

        alonzoBalance = evaluateTransactionBalance params utxo isNewPool (Alonzo.body txBody)
        -- babbageBalance params = evaluateTransactionBalance params (Cardano.toLedgerUTxO Cardano.ShelleyBasedEraBabbage utxo) isNewPool txBody

        -- | TODO: use pool ids info
        -- isNewPool :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> Bool
        isNewPool _kh = True -- StakePoolKeyHash kh `S.notMember` poolids

    withCheckUnits ::
         Alonzo.PParams Alonzo.Era
      -> Ledger.UTxO Alonzo.Era
      -> Alonzo.ValidatedTx Alonzo.Era
      -> (Alonzo.ExUnits -> Run (Either FailReason Stat))
      -> Run (Either FailReason Stat)
    withCheckUnits params utxo txBody cont = do
      slotCfg <- gets (bchConfigSlotConfig . bchConfig)
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
            (toAlonzoCostModels $ Alonzo._costmdls params)
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
getUTxO :: P.Tx -> Run (Maybe (Either String (Ledger.UTxO Alonzo.Era)))
getUTxO tx = do
  networkId <- bchConfigNetworkId <$> gets bchConfig
  mOuts <- sequence <$> mapM (getTxOut . P.txInRef) ins
  pure $ fmap (Alonzo.toUtxo networkId . zip (P.txInRef <$> ins)) mOuts
  where
    ins = S.toList $ P.txInputs tx

{-
toScriptData :: ToData a => a -> Cardano.ScriptData
toScriptData d = fromAlonzoData $ Alonzo.Data $ toData d
-}

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
applyTx stat tid etx@(Tx extra P.Tx {..}) = do
  updateUtxos
  updateRewards
  updateCertificates
  updateFees
  saveTx
  saveDatums
  where
    saveDatums = modify' $ \s -> s {bchDatums = txData <> bchDatums s}

    saveTx = do
      t <- gets bchCurrentSlot
      statPercent <- getStatPercent
      modify' $ \s -> s {bchTxs = appendLog t (TxStat etx t stat statPercent) $ bchTxs s}

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
        inRefSet = S.map P.txInRef ins
        inRefs = M.fromList $ (,()) . P.txInRef <$> S.toList ins
        rmIns a = M.difference a inRefs

    insertOut (ix, out) = do
      insertAddresses
      insertUtxos
      where
        ref = TxOutRef tid ix
        addr = txOutAddress out

        insertAddresses = modify' $ \s -> s {bchAddresses = M.alter (Just . maybe (S.singleton ref) (S.insert ref)) addr $ bchAddresses s}
        insertUtxos = modify' $ \s -> s {bchUtxos = M.singleton ref out <> bchUtxos s}

    updateRewards = mapM_ modifyWithdraw $ extra'withdraws extra
      where
        modifyWithdraw Withdraw{..} = onStake (withdrawStake withdraw'credential)

    updateCertificates = mapM_ (onStake . reactDCert . certificate'dcert) $ extra'certificates extra

    onStake f = modify' $ \st -> st { bchStake = f $ bchStake st }

    updateFees = do
      st <- gets bchStake
      forM_ (rewardStake (getLovelace txFee) st) $ \nextSt -> modify' $ \bch -> bch { bchStake = nextSt }

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: Address -> Run [TxOutRef]
txOutRefAt addr = txOutRefAtState addr <$> get

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: Address -> Blockchain -> [TxOutRef]
txOutRefAtState addr st = maybe [] S.toList . M.lookup addr $ bchAddresses st

-- | Get all UTXOs that belong to an address
utxoAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
utxoAt addr = utxoAtState addr <$> get

-- | Get all UTXOs that belong to an address
utxoAtState :: HasAddress user => user -> Blockchain -> [(TxOutRef, TxOut)]
utxoAtState (toAddress -> addr) st =
  mapMaybe (\r -> (r,) <$> M.lookup r (bchUtxos st)) refs
  where
    refs = txOutRefAtState addr st

-- | Reads typed datum from blockchain that belongs to UTXO (by reference).
datumAt :: FromData a => TxOutRef -> Run (Maybe a)
datumAt ref = do
  dhs <- gets bchDatums
  mDh <- (txOutDatumHash =<<) <$> getTxOut ref
  pure $ fromBuiltinData . getDatum =<< (`M.lookup` dhs) =<< mDh


-- | Reads current reward amount for a staking credential
rewardAt :: HasStakingCredential cred => cred -> Run Integer
rewardAt cred = gets (maybe 0 id . lookupReward (toStakingCredential cred) . bchStake)

-- | Returns all stakes delegatged to a pool
stakesAt :: PoolId -> Run [StakingCredential]
stakesAt (PoolId poolKey) = gets (lookupStakes (PoolId poolKey) . bchStake)

-- | Checks that pool is registered
hasPool :: PoolId -> Run Bool
hasPool (PoolId pkh) = gets (M.member (PoolId pkh) . stake'pools. bchStake)

-- | Checks that staking credential is registered
hasStake :: HasStakingCredential a => a -> Run Bool
hasStake key = gets (M.member (toStakingCredential key) . stake'stakes. bchStake)

getPools :: Run [PoolId]
getPools = gets (V.toList . stake'poolIds . bchStake)

----------------------------------------------------------------
-- logs

-- | Reads the log.
getLog :: Blockchain -> Log BchEvent
getLog Blockchain{..} =
  mconcat
    [ BchInfo <$> bchInfo
    , BchMustFailLog <$> mustFailLog
    , uncurry BchTx . (\tx@(txStatId -> ident) -> (txName ident, tx)) <$> bchTxs
    , BchFail <$> bchFails
    ]
  where
    txName = readTxName bchNames

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

