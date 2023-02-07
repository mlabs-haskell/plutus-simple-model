{-# LANGUAGE UndecidableInstances #-}

-- | Functions to create TXs and query blockchain model.
module Plutus.Model.Contract (
  -- * Modify blockchain
  newUser,
  sendTx,
  sendBlock,
  sendValue,
  withSpend,
  submitTx,
  waitNSlots,
  wait,
  waitUntil,

  -- * Query blockchain
  withMay,
  withMayBy,
  UserSpend (..),
  getHeadRef,
  spend,
  spend',
  noErrors,
  valueAt,
  refValueAt,
  withUtxo,
  utxoAt,
  withDatum,
  datumAt,
  rewardAt,
  stakesAt,
  hasPool,
  hasStake,
  TxBox (..),
  txBoxAddress,
  txBoxDatumHash,
  txBoxValue,
  boxAt,
  nftAt,
  withBox,
  withNft,
  currentSlot,
  currentTime,
  getPrettyAddress,

  -- * Build TX
  signTx,
  DatumMode (..),
  payToKey,
  payToKeyDatum,
  payToScript,
  loadRefScript,
  loadRefScriptDatum,
  payToRef,
  payFee,
  userSpend,
  spendPubKey,
  spendScript,
  spendScriptRef,
  spendBox,
  refInputInline,
  refInputHash,
  refBoxInline,
  refBoxHash,
  collateralInput,
  readOnlyBox,
  modifyBox,
  mintValue,
  validateIn,

  -- ** Staking validators primitives

  --

  -- | to use them convert vanila Plutus @Tx@ to @Tx@ with @toExtra@
  Tx (..),
  toExtra,
  HasStakingCredential (..),
  withdrawStakeKey,
  withdrawStakeScript,
  registerStakeKey,
  registerStakeScript,
  deregisterStakeKey,
  deregisterStakeScript,
  registerPool,
  retirePool,
  insertPool,
  deletePool,
  delegateStakeKey,
  delegateStakeScript,

  -- * time helpers (converts to POSIXTime milliseconds)
  weeks,
  days,
  hours,
  minutes,
  seconds,
  millis,
  currentTimeInterval,
  currentTimeRad,

  -- * testing helpers
  mustFail,
  mustFailWith,
  mustFailWithName,
  checkErrors,
  testNoErrors,
  testNoErrorsTrace,
  testLimits,
  logBalanceSheet,

  -- * balance checks
  BalanceDiff,
  checkBalance,
  checkBalanceBy,
  HasAddress (..),
  owns,
  gives,
) where

import Control.Monad.State.Strict
import Prelude

import Data.Bifunctor (second)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq (drop, length)
import Data.Set (Set)
import Data.Set qualified as S

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Cardano.Simple.Ledger.Slot (Slot (..))
import Cardano.Simple.Ledger.TimeSlot (posixTimeToEnclosingSlot, slotToEndPOSIXTime)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2 hiding (Map)
import PlutusTx.Prelude qualified as Plutus

import Cardano.Simple.Ledger.Tx qualified as Fork
import Cardano.Simple.Ledger.Tx qualified as P
import Cardano.Simple.TxExtra
import Plutus.Model.Ada (Ada (..))
import Plutus.Model.Mock
import Plutus.Model.Pretty
import Plutus.Model.Stake qualified as Stake
import Plutus.Model.Validator as X
import Prettyprinter (Doc, indent, pretty, vcat, (<+>))

------------------------------------------------------------------------
-- modify blockchain

{- | Create new user with given amount of funds.
 It sends funds from the main admin user. Note that the admin
 should have those funds otherwise it will fail. Allocation of the funds
 for admin happens at the function @initMock@.
-}
newUser :: Value -> Run PubKeyHash
newUser val = do
  pkh <- emptyUser
  when (val /= mempty) $ do
    admin <- getMainUser
    sendValue admin val pkh
  pure pkh
  where
    emptyUser = do
      userCount <- gets mockUserStep
      let user = intToUser userCount
          pkh = userPubKeyHash user
          addr = pubKeyHashAddress pkh
          userNo = "User " <> show userCount
      modify' $ \s -> s {mockUserStep = userCount + 1, mockUsers = M.insert pkh user (mockUsers s)}
      writeUserName pkh userNo >> writeAddressName addr userNo
      pure pkh

-- | Sends value from one user to another.
sendValue :: PubKeyHash -> Value -> PubKeyHash -> Run ()
sendValue fromPkh amt toPkh = do
  mVal <- spend' fromPkh amt
  case mVal of
    Just val -> void $ sendTx =<< signTx fromPkh (toTx val)
    Nothing -> logFail (NotEnoughFunds fromPkh amt)
  where
    toTx sp = userSpend sp <> payToKey toPkh amt

-- | Spend or fail if there are no funds
withSpend :: PubKeyHash -> Value -> (UserSpend -> Run ()) -> Run ()
withSpend pkh val cont = do
  mUsp <- spend' pkh val
  case mUsp of
    Just usp -> cont usp
    Nothing -> logError "No funds for user to spend"

-- | Signs transaction and sends it ignoring the result stats.
submitTx :: PubKeyHash -> Tx -> Run ()
submitTx pkh tx = void $ sendTx =<< signTx pkh tx

------------------------------------------------------------------------
-- query blockchain

-- | Current slot of blockchain.
currentSlot :: Run Slot
currentSlot = gets mockCurrentSlot

-- | Current time of blockchain
currentTime :: Run POSIXTime
currentTime = do
  slotCfg <- gets (mockConfigSlotConfig . mockConfig)
  slotToEndPOSIXTime slotCfg <$> currentSlot

{- | Waits for specified amount of time.
 It makes blockchain to progress corresponding number of slots.
-}
wait :: POSIXTime -> Run ()
wait time = do
  slotCfg <- gets (mockConfigSlotConfig . mockConfig)
  waitNSlots $ posixTimeToEnclosingSlot slotCfg time

{- | Waits until the specified time.
 It makes blockchain to progress corresponding number of slots.
-}
waitUntil :: POSIXTime -> Run ()
waitUntil time = do
  slot <- currentSlot
  slotCfg <- gets (mockConfigSlotConfig . mockConfig)
  waitNSlots $ posixTimeToEnclosingSlot slotCfg time - slot

-- | blockchain runs without errors, all submitted transactions were accepted.
noErrors :: Run Bool
noErrors = nullLog <$> gets mockFails

{- | Get total value on the address or user by @PubKeyHash@

Since 0.4 this includes UTxOs that have reference scripts.
-}
valueAt :: HasAddress user => user -> Run Value
valueAt user = foldMap (txOutValue . snd) <$> utxoAt user

-- | Get total value on the address or user by address on reference script UTXOs.
refValueAt :: HasAddress user => user -> Run Value
refValueAt user = foldMap (txOutValue . snd) <$> refScriptAt user

valueAtState :: HasAddress user => user -> Mock -> Value
valueAtState user = valueAtStateBy mockUtxos user <> valueAtStateBy mockRefScripts user

-- | Get total value on the address or user by @PubKeyHash@.
valueAtStateBy :: HasAddress user => (Mock -> Map TxOutRef TxOut) -> user -> Mock -> Value
valueAtStateBy extract user st = foldMap (txOutValue . snd) $ utxoAtStateBy extract user st

{- | To spend some value user should provide valid set of UTXOs owned by the user.
 Also it holds the change. For example if user has one UTXO that holds 100 coins
 and wants to spend 20 coins, user provides TxOut for change of 80 coins that are paid
 back to the user.
-}
data UserSpend = UserSpend
  { userSpend'inputs :: Set Fork.TxIn
  , userSpend'change :: Maybe TxOut
  }
  deriving (Show)

{- | Reads first @TxOutRef@ from user spend inputs.
 It can be useful to create NFTs that depend on TxOutRef's.
-}
getHeadRef :: UserSpend -> TxOutRef
getHeadRef UserSpend {..} = Fork.txInRef $ S.elemAt 0 userSpend'inputs

-- | Variant of spend' that fails in run-time if there are not enough funds to spend.
spend :: PubKeyHash -> Value -> Run UserSpend
spend pkh val = do
  mSp <- spend' pkh val
  pure $ fromJust mSp

{- | Generate a transaction to spend value.

Does not submit the transaction, or mark UTxOs as spent.

Will not spend value from a UTxO if it contains a reference script, so as not to
  make the script unreferencable.

  Since 0.5, It will also not spend outputs that hold datums.
-}
spend' :: PubKeyHash -> Value -> Run (Maybe UserSpend)
spend' pkh expected = do
  refs <- txOutRefAt (pubKeyHashAddress pkh)
  mUtxos <-
    gets
      ( (\m -> mapM (\r -> (r,) <$> M.lookup r m) refs)
          . M.filter
            ( \txo ->
                isNothing (txOutReferenceScript txo)
                  && (== NoOutputDatum) (txOutDatum txo)
            )
          . mockUtxos
      )
  case mUtxos of
    Just utxos -> pure $ toRes $ foldl go (expected, []) utxos
    Nothing -> pure Nothing
  where
    go (curVal, resUtxos) u@(_, out)
      | curVal `leq` mempty = (curVal, resUtxos)
      | nextVal `lt'` curVal = (nextVal, u : resUtxos)
      | otherwise = (curVal, resUtxos)
      where
        outVal = txOutValue out
        nextVal = snd $ split $ curVal <> Plutus.negate outVal
        -- 'lt' seems to be not usable here, see
        -- https://github.com/mlabs-haskell/plutus-simple-model/issues/26 for details.
        -- Strictly speaking, @isZero neg@ is redundant here, it always should hold
        -- be the way @nextVal@ is constructed. But general **less then** must
        -- check the negative part is empty, so I decided to keep it for clarity.
        lt' :: Value -> Value -> Bool
        lt' a b = not (isZero pos) && isZero neg
          where
            (neg, pos) = split $ b <> Plutus.negate a

    toRes (curVal, utxos)
      | curVal `leq` mempty = Just $ UserSpend (foldMap (S.singleton . toInput) utxos) (getChange utxos)
      | otherwise = Nothing

    toInput (ref, _) = Fork.TxIn ref (Just Fork.ConsumePublicKeyAddress)

    getChange utxos
      | change /= mempty = Just $ TxOut (pubKeyHashAddress pkh) change NoOutputDatum Nothing
      | otherwise = Nothing
      where
        change = foldMap (txOutValue . snd) utxos <> Plutus.negate expected

------------------------------------------------------------------------
-- datum mode

-- | How to store datum (as Hash or Inlined to TxOut)
data DatumMode a
  = InlineDatum a
  | HashDatum a
  deriving (Show, Eq)

-- | Convert DatumMode to pieces of TXs related to datum storage
fromDatumMode :: ToData a => DatumMode a -> (OutputDatum, Map DatumHash Datum)
fromDatumMode = \case
  HashDatum dat ->
    let dh = datumHash datum
        datum = Datum $ toBuiltinData dat
     in (OutputDatumHash dh, M.singleton dh datum)
  InlineDatum dat ->
    let datum = Datum $ toBuiltinData dat
     in (OutputDatum datum, M.empty)

------------------------------------------------------------------------
-- build Tx

-- | Pay to public key with datum
payToKeyDatum :: ToData a => PubKeyHash -> DatumMode a -> Value -> Tx
payToKeyDatum pkh dat val =
  toExtra $
    mempty
      { P.txOutputs = [TxOut (pubKeyHashAddress pkh) val outDatum Nothing]
      , P.txData = datumMap
      }
  where
    (outDatum, datumMap) = fromDatumMode dat

{- | Pay value to the owner of PubKeyHash.
 We use address to supply staking credential if we need it.
-}
payToKey :: HasAddress pubKeyHash => pubKeyHash -> Value -> Tx
payToKey pkh val =
  toExtra $
    mempty
      { P.txOutputs = [TxOut (toAddress pkh) val NoOutputDatum Nothing]
      }

{- | Pay to the script.
 We can use TypedValidator as argument and it will be checked that the datum is correct.
-}
payToScript ::
  (HasDatum script, HasAddress script) =>
  script ->
  DatumMode (DatumType script) ->
  Value ->
  Tx
payToScript script dat val =
  toExtra $
    mempty
      { P.txOutputs = [TxOut (toAddress script) val outDatum Nothing]
      , P.txData = datumMap
      }
  where
    (outDatum, datumMap) = fromDatumMode dat

-- | Uploads the reference script to blockchain
loadRefScript :: (IsValidator script) => script -> Value -> Tx
loadRefScript script = loadRefScriptBy script Nothing

-- | Uploads the reference script to blockchain
loadRefScriptDatum :: (IsValidator script) => script -> DatumMode (DatumType script) -> Value -> Tx
loadRefScriptDatum script dat = loadRefScriptBy script (Just dat)

-- | Uploads the reference script to blockchain
loadRefScriptBy ::
  (IsValidator script) =>
  script ->
  Maybe (DatumMode (DatumType script)) ->
  Value ->
  Tx
loadRefScriptBy script mDat val =
  toExtra $
    mempty
      { P.txOutputs = [TxOut (toAddress script) val outDatum (Just sh)]
      , P.txData = datumMap
      , P.txScripts = M.singleton sh validator
      }
  where
    sh = scriptHash script
    validator = toVersionedScript script
    (outDatum, datumMap) = maybe (NoOutputDatum, M.empty) fromDatumMode mDat

-- | Pays to the TxOut that references some script stored on ledger
payToRef ::
  (HasAddress script, HasDatum script) =>
  script ->
  DatumMode (DatumType script) ->
  Value ->
  Tx
payToRef script dat val =
  toExtra $
    mempty
      { P.txOutputs = [TxOut (toAddress script) val outDatum Nothing]
      , P.txData = datumMap
      }
  where
    (outDatum, datumMap) = fromDatumMode dat

-- | Pay fee for TX-submission
payFee :: Ada -> Tx
payFee val =
  toExtra $
    mempty
      { P.txFee = val
      }

-- | Spend @TxOutRef@ that belongs to pub key (user).
spendPubKey :: TxOutRef -> Tx
spendPubKey ref =
  toExtra $
    mempty
      { P.txInputs = S.singleton $ Fork.TxIn ref (Just Fork.ConsumePublicKeyAddress)
      }

-- | Spend script input.
spendScript ::
  (IsValidator script) =>
  script ->
  TxOutRef ->
  RedeemerType script ->
  DatumType script ->
  Tx
spendScript tv ref red dat =
  toExtra $
    mempty
      { P.txInputs = S.singleton $ Fork.TxIn ref (Just $ Fork.ConsumeScriptAddress (Just $ Versioned (getLanguage tv) (toValidator tv)) (toRedeemer red) (toDatum dat))
      }

-- | Spends script that references other script
spendScriptRef ::
  (IsValidator script) =>
  TxOutRef ->
  script ->
  TxOutRef ->
  RedeemerType script ->
  DatumType script ->
  Tx
spendScriptRef refScript script refOut red dat =
  toExtra $
    mempty
      { P.txInputs = S.singleton $ Fork.TxIn refOut (Just $ Fork.ConsumeScriptAddress Nothing (toRedeemer red) (toDatum dat))
      , P.txReferenceInputs = S.singleton $ Fork.TxIn refScript Nothing
      , P.txScripts = M.singleton sh validator
      }
  where
    sh = scriptHash script
    validator = toVersionedScript script

-- | Reference input with inlined datum
refInputInline :: TxOutRef -> Tx
refInputInline ref =
  toExtra $
    mempty
      { P.txReferenceInputs = S.singleton $ Fork.TxIn ref Nothing
      }

-- | Reference input with hashed datum
refInputHash :: ToData datum => TxOutRef -> datum -> Tx
refInputHash ref dat =
  toExtra $
    mempty
      { P.txReferenceInputs = S.singleton $ Fork.TxIn ref Nothing
      , P.txData = M.singleton dh datum
      }
  where
    dh = datumHash datum
    datum = Datum $ toBuiltinData dat

-- | Set collateral input
collateralInput :: TxOutRef -> Tx
collateralInput ref =
  toExtra $
    mempty
      { P.txCollateral = S.singleton $ Fork.TxIn ref Nothing
      }

-- | Reference box with inlined datum
refBoxInline :: TxBox script -> Tx
refBoxInline = refInputInline . txBoxRef

-- | Reference box with hashed datum
refBoxHash :: IsValidator script => TxBox script -> DatumType script -> Tx
refBoxHash = refInputHash . txBoxRef

-- | Spend script input.
spendBox ::
  (IsValidator script) =>
  script ->
  RedeemerType script ->
  TxBox script ->
  Tx
spendBox tv red TxBox {..} =
  spendScript tv txBoxRef red txBoxDatum

-- | Specify that box is used as oracle (read-only). Spends value to itself and uses the same datum.
readOnlyBox ::
  (IsValidator script) =>
  script ->
  TxBox script ->
  RedeemerType script ->
  Tx
readOnlyBox tv box act = modifyBox tv box act HashDatum id

-- | Modifies the box. We specify how script box datum and value are updated.
modifyBox ::
  (IsValidator script) =>
  script ->
  TxBox script ->
  RedeemerType script ->
  (DatumType script -> DatumMode (DatumType script)) ->
  (Value -> Value) ->
  Tx
modifyBox tv box act modDatum modValue =
  mconcat
    [ spendBox tv act box
    , payToScript tv (modDatum $ txBoxDatum box) (modValue $ txBoxValue box)
    ]

-- | Spend value for the user and also include change in the outputs.
userSpend :: UserSpend -> Tx
userSpend (UserSpend ins mChange) =
  toExtra $
    mempty
      { P.txInputs = ins
      , P.txOutputs = foldMap pure mChange
      }

mintTx :: Mint -> Tx
mintTx m = mempty {tx'extra = mempty {extra'mints = [m]}}

-- | Mints value. To use redeemer see function @addMintRedeemer@.
mintValue ::
  (ToData redeemer) =>
  TypedPolicy redeemer ->
  redeemer ->
  Value ->
  Tx
mintValue (TypedPolicy policy) redeemer val =
  mintTx (Mint val (Redeemer $ toBuiltinData redeemer) policy)

-- | Set validation time
validateIn :: POSIXTimeRange -> Tx -> Run Tx
validateIn times = updatePlutusTx $ \tx -> do
  slotCfg <- gets (mockConfigSlotConfig . mockConfig)
  pure $
    tx
      { P.txValidRange = Plutus.fmap (posixTimeToEnclosingSlot slotCfg) times
      }

----------------------------------------------------------------------
-- queries

-- | Typed txOut that contains decoded datum typed to script/validator
data TxBox script = TxBox
  { txBoxRef :: TxOutRef
  -- ^ tx out reference
  , txBoxOut :: TxOut
  -- ^ tx out
  , txBoxDatum :: DatumType script
  -- ^ datum
  }

deriving instance Show (DatumType a) => Show (TxBox a)
deriving instance Eq (DatumType a) => Eq (TxBox a)

instance HasAddress (TxBox a) where
  toAddress = txBoxAddress

-- | Get box address
txBoxAddress :: TxBox a -> Address
txBoxAddress = txOutAddress . txBoxOut

-- | Get box datum hash
txBoxDatumHash :: TxBox a -> Maybe DatumHash
txBoxDatumHash = txOutDatumHash . txBoxOut

-- | Get value at the box.
txBoxValue :: TxBox a -> Value
txBoxValue = txOutValue . txBoxOut

{- | Read UTXOs with datums.

Since 0.4 this includes UTxOs that have reference scripts.
-}
boxAt :: (IsValidator script) => script -> Run [TxBox script]
boxAt addr = do
  utxos <- utxoAt (toAddress addr)
  catMaybes <$> mapM (\(ref, tout) -> fmap (TxBox ref tout) <$> datumAt ref) utxos

{- | It expects that Typed validator can have only one UTXO
 which is NFT.
-}
nftAt :: IsValidator script => script -> Run (TxBox script)
nftAt tv = head <$> boxAt tv

-- | Safe query for single Box
withBox :: IsValidator script => (TxBox script -> Bool) -> script -> (TxBox script -> Run ()) -> Run ()
withBox isBox script =
  withMayBy readMsg (L.find isBox <$> boxAt script)
  where
    readMsg = ("No UTxO box for: " <>) <$> getPrettyAddress (toAddress script)

-- | Reads single box from the list. we expect NFT to be a single UTXO for a given script.
withNft :: IsValidator script => script -> (TxBox script -> Run ()) -> Run ()
withNft = withBox (const True)

----------------------------------------------------------------------
-- time helpers

-- | Convert amount of milliseconds to POSIXTime
millis :: Integer -> POSIXTime
millis = POSIXTime

-- | Convert amount of seconds to POSIXTime
seconds :: Integer -> POSIXTime
seconds n = millis (1000 * n)

-- | Convert amount of minutes to POSIXTime
minutes :: Integer -> POSIXTime
minutes n = seconds (60 * n)

-- | Convert amount of hours to POSIXTime
hours :: Integer -> POSIXTime
hours n = minutes (60 * n)

-- | Convert amount of days to POSIXTime
days :: Integer -> POSIXTime
days n = hours (24 * n)

-- | Convert amount of weeks to POSIXTime
weeks :: Integer -> POSIXTime
weeks n = days (7 * n)

-- | places interval around current time
currentTimeInterval :: POSIXTime -> POSIXTime -> Run POSIXTimeRange
currentTimeInterval minTime maxTime = do
  time <- currentTime
  pure $ interval (time + minTime) (time + maxTime)

-- | Valid time range with given radius around current time
currentTimeRad :: POSIXTime -> Run POSIXTimeRange
currentTimeRad timeRad = currentTimeInterval (negate timeRad) timeRad

----------------------------------------------------------------------
-- testing helpers

{- | Try to execute an action, and if it fails, restore to the current state
 while preserving logs. If the action succeeds, logs an error as we expect
 it to fail. Use 'mustFailWith' and 'mustFailWithBlock' to provide custom
 error message or/and failure action name.
-}
mustFail :: Run a -> Run ()
mustFail = mustFailWith "Expected action to fail but it succeeds"

-- | The same as 'mustFail', but takes custom error message.
mustFailWith :: String -> Run a -> Run ()
mustFailWith = mustFailWithName "Unnamed failure action"

-- | The same as 'mustFail', but takes action name and custom error message.
mustFailWithName :: String -> String -> Run a -> Run ()
mustFailWithName name msg act = do
  st <- get
  preFails <- getFails
  void act
  postFails <- getFails
  if noNewErrors preFails postFails
    then logError msg
    else do
      infoLog <- gets mockInfo
      put
        st
          { mockInfo = infoLog
          , mustFailLog = mkMustFailLog preFails postFails
          }
  where
    noNewErrors (fromLog -> a) (fromLog -> b) = length a == length b
    mkMustFailLog (unLog -> pre) (unLog -> post) =
      Log $ second (MustFailLog name) <$> Seq.drop (Seq.length pre) post

{- | Checks that script runs without errors and returns pretty printed failure
 if something bad happens.
-}
checkErrors :: Run (Maybe String)
checkErrors = do
  failures <- fromLog <$> getFails
  names <- gets mockNames
  pure $
    if null failures
      then Nothing
      else Just (init . unlines $ fmap (ppFailure names) failures)

{- | like 'testNoErrors' but prints out blockchain log for both
 failing and successful tests. The recommended way to choose
 between those two is using @tasty@ 'askOption'. To pull in
 parameters use an 'Ingredient' built with 'includingOptions'.
-}
testNoErrorsTrace :: Value -> MockConfig -> String -> Run a -> TestTree
testNoErrorsTrace funds cfg msg act =
  testCaseInfo msg
    $ maybe
      (pure mockLog)
      assertFailure
    $ errors >>= \errs -> pure $ errs <> mockLog
  where
    (errors, mock) = runMock (act >> checkErrors) $ initMock cfg funds
    mockLog = "\nBlockchain log :\n----------------\n" <> ppMockEvent (mockNames mock) (getLog mock)

-- | Logs the blockchain state, i.e. balance sheet in the log
logBalanceSheet :: Run ()
logBalanceSheet =
  modify' $ \s -> s {mockInfo = appendLog (mockCurrentSlot s) (ppBalanceSheet s) (mockInfo s)}

testNoErrors :: Value -> MockConfig -> String -> Run a -> TestTree
testNoErrors funds cfg msg act =
  testCase msg $
    maybe (pure ()) assertFailure $
      fst (runMock (act >> checkErrors) (initMock cfg funds))

-- | check transaction limits
testLimits :: Value -> MockConfig -> String -> (Log TxStat -> Log TxStat) -> Run a -> TestTree
testLimits initFunds cfg msg tfmLog act =
  testCase msg $ assertBool limitLog isOk
  where
    (isOk, mock) = runMock (act >> noErrors) (initMock (warnLimits cfg) initFunds)
    limitLog = ppLimitInfo (mockNames mock) $ tfmLog $ mockTxs mock

----------------------------------------------------------------------
-- balance diff

-- | Balance difference. If user/script spends value it is negative if gains it is positive.
newtype BalanceDiff = BalanceDiff (Map Address Value)

instance Semigroup BalanceDiff where
  (<>) (BalanceDiff ma) (BalanceDiff mb) = BalanceDiff $ M.unionWith (<>) ma mb

instance Monoid BalanceDiff where
  mempty = BalanceDiff mempty

-- | Checks that after execution of an action balances changed in certain way
checkBalance :: BalanceDiff -> Run a -> Run a
checkBalance diff = checkBalanceBy (const diff)

-- | Checks that after execution of an action balances changed in certain way
checkBalanceBy :: (a -> BalanceDiff) -> Run a -> Run a
checkBalanceBy getDiffs act = do
  beforeSt <- get
  res <- act
  let BalanceDiff diffs = getDiffs res
      addrs = M.keys diffs
      before = fmap (`valueAtState` beforeSt) addrs
  after <- mapM valueAt addrs
  mapM_ (logError . show . vcat <=< mapM ppError) (check addrs diffs before after)
  pure res
  where
    ppError :: (Address, Value, Value) -> Run (Doc ann)
    ppError (addr, expected, got) = do
      names <- gets mockNames
      let addrName = maybe (pretty addr) pretty $ readAddressName names addr
      pure $
        vcat
          [ "Balance error for:" <+> addrName
          , indent 2 $
              vcat
                [ "Expected:" <+> ppBalanceWith names expected
                , "Got:" <+> ppBalanceWith names got
                ]
          ]

    check :: [Address] -> Map Address Value -> [Value] -> [Value] -> Maybe [(Address, Value, Value)]
    check addrs diffs before after
      | null errs = Nothing
      | otherwise = Just errs
      where
        errs = catMaybes $ zipWith3 go addrs before after

        go addr a b
          | res Plutus.== dv = Nothing
          | otherwise = Just (addr, dv, res)
          where
            res = b <> Plutus.negate a
            dv = diffs M.! addr

-- | Balance difference constructor
owns :: HasAddress user => user -> Value -> BalanceDiff
owns user val = BalanceDiff $ M.singleton (toAddress user) val

-- | User A gives value to user B.
gives :: (HasAddress userA, HasAddress userB) => userA -> Value -> userB -> BalanceDiff
gives userA val userB = owns userA (Plutus.negate val) <> owns userB val

-----------------------------------------------------------
-- staking and certificates

-- | Construct Tx from withdraw parts
withdrawTx :: Withdraw -> Tx
withdrawTx w = mempty {tx'extra = mempty {extra'withdraws = [w]}}

-- | Convert to internal redeemer
toRedeemer :: ToData red => red -> Redeemer
toRedeemer = Redeemer . toBuiltinData

-- | Convert to internal datum
toDatum :: ToData dat => dat -> Datum
toDatum = Datum . toBuiltinData

withStakeScript ::
  (ToData red) =>
  TypedStake red ->
  red ->
  Maybe (Redeemer, Versioned StakeValidator)
withStakeScript (TypedStake script) red = Just (toRedeemer red, script)

-- | Add staking withdrawal based on pub key hash
withdrawStakeKey :: PubKeyHash -> Ada -> Tx
withdrawStakeKey key (Lovelace amount) =
  withdrawTx $
    Withdraw (keyToStaking key) amount Nothing

-- | Add staking withdrawal based on script
withdrawStakeScript ::
  (ToData redeemer) =>
  TypedStake redeemer ->
  redeemer ->
  Ada ->
  Tx
withdrawStakeScript (TypedStake validator) red (Lovelace amount) =
  withdrawTx $
    Withdraw (scriptToStaking validator) amount (withStakeScript (TypedStake validator) red)

certTx :: Certificate -> Tx
certTx cert = mempty {tx'extra = mempty {extra'certificates = [cert]}}

-- | Register staking credential by key
registerStakeKey :: PubKeyHash -> Tx
registerStakeKey pkh =
  certTx $
    Certificate (DCertDelegRegKey $ keyToStaking pkh) Nothing

-- NOTE: that according to cardano-ledger code we need to provide script witness
-- only for two cases:
--   * DCertDeleg + DeRegKey
--   * DCertDeleg + Delegate
--
-- if we provide for other cases it will fail with exception RedeemerNotNeeded
-- It means that we have to omit script witness for DCertDelegRegKey

-- | Register staking credential by stake validator
registerStakeScript ::
  TypedStake redeemer -> Tx
registerStakeScript script =
  certTx $
    Certificate (DCertDelegRegKey $ toStakingCredential script) Nothing

-- | DeRegister staking credential by key
deregisterStakeKey :: PubKeyHash -> Tx
deregisterStakeKey pkh =
  certTx $
    Certificate (DCertDelegDeRegKey $ keyToStaking pkh) Nothing

-- | DeRegister staking credential by stake validator
deregisterStakeScript ::
  (ToData redeemer) =>
  TypedStake redeemer ->
  redeemer ->
  Tx
deregisterStakeScript script red =
  certTx $
    Certificate (DCertDelegDeRegKey $ toStakingCredential script) (withStakeScript script red)

{- | Register staking pool
 TODO: thois does not work on TX level.
 Use insertPool as a workaround.
-}
registerPool :: PoolId -> Tx
registerPool (PoolId pkh) =
  certTx $
    Certificate (DCertPoolRegister pkh pkh) Nothing

-- | Insert pool id to the list of stake pools
insertPool :: PoolId -> Run ()
insertPool pid = modify' $ \st ->
  st {mockStake = Stake.regPool pid $ mockStake st}

-- | delete pool from the list of stake pools
deletePool :: PoolId -> Run ()
deletePool pid = modify' $ \st ->
  st {mockStake = Stake.retirePool pid $ mockStake st}

-- | Retire staking pool
retirePool :: PoolId -> Tx
retirePool (PoolId pkh) =
  certTx $
    Certificate (DCertPoolRetire pkh 0) Nothing

-- | Delegates staking credential (specified by key) to pool
delegateStakeKey :: PubKeyHash -> PoolId -> Tx
delegateStakeKey stakeKey (PoolId poolKey) =
  certTx $
    Certificate (DCertDelegDelegate (keyToStaking stakeKey) poolKey) Nothing

-- | Delegates staking credential (specified by stakevalidator) to pool
delegateStakeScript ::
  (ToData redeemer) =>
  TypedStake redeemer ->
  redeemer ->
  PoolId ->
  Tx
delegateStakeScript script red (PoolId poolKey) =
  certTx $
    Certificate (DCertDelegDelegate (toStakingCredential script) poolKey) (withStakeScript script red)
