{-# LANGUAGE UndecidableInstances #-}
-- | Functions to create TXs and query blockchain model.
module Plutus.Test.Model.Contract (
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
  UserSpend (..),
  getHeadRef,
  spend,
  spend',
  noErrors,
  valueAt,
  utxoAt,
  datumAt,
  rewardAt,
  stakesAt,
  hasPool,
  hasStake,
  TxBox(..),
  txBoxAddress,
  txBoxDatumHash,
  txBoxValue,
  boxAt,
  scriptBoxAt,
  nftAt,
  currentSlot,
  currentTime,

  -- * Build TX
  signTx,
  payToPubKey,
  payWithDatumToPubKey,
  payToScript,
  payToScriptAddress,
  payToPubKeyAddress,
  payFee,
  userSpend,
  spendPubKey,
  spendScript,
  spendBox,
  readOnlyBox,
  modifyBox,
  mintValue,
  addMintRedeemer,
  validateIn,

  -- ** Staking valdiators primitives
  --
  -- | to use them convert vanila Plutus @Tx@ to @Tx@ with @toExtra@
  Tx(..),
  toExtra,
  HasStakingCredential(..),
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
  HasAddress(..),
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
import Data.Set (Set)
import Data.Set qualified as S
import Data.Sequence qualified as Seq (drop, length)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (datumHash)
import Ledger.TimeSlot (posixTimeToEnclosingSlot, slotToEndPOSIXTime)
import Ledger.Typed.Scripts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval ()
import Plutus.V1.Ledger.Slot
import Plutus.V1.Ledger.Tx hiding (Tx(Tx))
import Plutus.V1.Ledger.Value
import PlutusTx.Prelude qualified as Plutus

import Plutus.Test.Model.Blockchain
import Plutus.Test.Model.Fork.TxExtra
import Plutus.Test.Model.Pretty
import Prettyprinter (Doc, vcat, indent, (<+>), pretty)
import Plutus.Test.Model.Stake qualified as Stake

------------------------------------------------------------------------
-- modify blockchain

{- | Create new user with given amount of funds.
 It sends funds from the main admin user. Note that the admin
 should have those funds otherwise it will fail. Allocation of the funds
 for admin happens at the function @initBch@.
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
      userCount <- gets bchUserStep
      let pk = intToPubKey userCount
          pkh = pubKeyHash pk
          addr = pubKeyHashAddress pkh
          userNo = "User " ++ show userCount
      modify' $ \s -> s {bchUserStep = userCount + 1, bchUsers = M.insert pkh (User pk) (bchUsers s)}
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
    toTx sp = userSpend sp <> payToPubKey toPkh amt

-- | Spend or fail if there are no funds
withSpend :: PubKeyHash -> Value -> (UserSpend -> Run ()) -> Run ()
withSpend pkh val cont = do
  mUsp <- spend' pkh val
  case mUsp of
    Just usp -> cont usp
    Nothing  -> logError "No funds for user to spend"

-- | Signs transaction and sends it ignoring the result stats.
submitTx :: PubKeyHash -> Tx -> Run ()
submitTx pkh tx = void $ sendTx =<< signTx pkh tx

------------------------------------------------------------------------
-- query blockchain

-- | Current slot of blockchain.
currentSlot :: Run Slot
currentSlot = gets bchCurrentSlot

-- | Current time of blockchain
currentTime :: Run POSIXTime
currentTime = do
  slotCfg <- gets (bchConfigSlotConfig . bchConfig)
  slotToEndPOSIXTime slotCfg <$> currentSlot

{- | Waits for specified amount of time.
 It makes blockchain to progress corresponding number of slots.
-}
wait :: POSIXTime -> Run ()
wait time = do
  slotCfg <- gets (bchConfigSlotConfig . bchConfig)
  waitNSlots $ posixTimeToEnclosingSlot slotCfg time

{- | Waits until the specified time.
 It makes blockchain to progress corresponding number of slots.
-}
waitUntil :: POSIXTime -> Run ()
waitUntil time = do
  slot <- currentSlot
  slotCfg <- gets (bchConfigSlotConfig . bchConfig)
  waitNSlots $ posixTimeToEnclosingSlot slotCfg time - slot

-- | blockhain runs without errors, all submited transactions were accepted.
noErrors :: Run Bool
noErrors = nullLog <$> gets bchFails

-- | Get total value on the address or user by @PubKeyHash@.
valueAt :: HasAddress user => user -> Run Value
valueAt user = foldMap (txOutValue . snd) <$> utxoAt user

{- | To spend some value user should provide valid set of UTXOs owned by the user.
 Also it holds the change. For example if user has one UTXO that holds 100 coins
 and wants to spend 20 coins, user provides TxOut for change of 80 coins that are paid
 back to the user.
-}
data UserSpend = UserSpend
  { userSpend'inputs :: Set TxIn
  , userSpend'change :: Maybe TxOut
  }
  deriving (Show)

-- | Reads first @TxOutRef@ from user spend inputs.
-- It can be useful to create NFTs that depend on TxOutRef's.
getHeadRef :: UserSpend -> TxOutRef
getHeadRef UserSpend{..} = txInRef $ S.elemAt 0 userSpend'inputs

-- | Variant of spend' that fails in run-time if there are not enough funds to spend.
spend :: PubKeyHash -> Value -> Run UserSpend
spend pkh val = do
  mSp <- spend' pkh val
  pure $ fromJust mSp

{- | User wants to spend money.
 It returns input UTXOs and output UTXOs for change.
 Note that it does not removes UTXOs from user account.
 We can only spend by submitting TXs, so if you run it twice
 it will choose from the same set of UTXOs.
-}
spend' :: PubKeyHash -> Value -> Run (Maybe UserSpend)
spend' pkh expected = do
  refs <- txOutRefAt (pubKeyHashAddress pkh)
  mUtxos <- fmap (\m -> mapM (\r -> (r,) <$> M.lookup r m) refs) $ gets bchUtxos
  case mUtxos of
    Just utxos -> pure $ toRes $ foldl go (expected, []) utxos
    Nothing -> pure Nothing
  where
    go (curVal, resUtxos) u@(_, out)
      | curVal `leq` mempty = (curVal, resUtxos)
      | nextVal `lt` curVal = (nextVal, u : resUtxos)
      | otherwise = (curVal, resUtxos)
      where
        outVal = txOutValue out
        nextVal = snd $ split $ curVal <> Plutus.negate outVal

    toRes (curVal, utxos)
      | curVal `leq` mempty = Just $ UserSpend (foldMap (S.singleton . toInput) utxos) (getChange utxos)
      | otherwise = Nothing

    toInput (ref, _) = TxIn ref (Just ConsumePublicKeyAddress)

    getChange utxos
      | change /= mempty = Just $ TxOut (pubKeyHashAddress pkh) change Nothing
      | otherwise = Nothing
      where
        change = foldMap (txOutValue . snd) utxos <> Plutus.negate expected

------------------------------------------------------------------------
-- build Tx

-- | Pay value to the owner of PubKeyHash.
payToPubKey :: PubKeyHash -> Value -> Tx
payToPubKey pkh val = toExtra $
  mempty
    { txOutputs = [TxOut (pubKeyHashAddress pkh) val Nothing]
    }

payWithDatumToPubKey :: ToData a => PubKeyHash -> a -> Value -> Tx
payWithDatumToPubKey pkh dat val = toExtra $
  mempty
    { txOutputs = [TxOut (pubKeyHashAddress pkh) val (Just dh)]
    , txData = M.singleton dh datum
    }
  where
    dh = datumHash datum
    datum = Datum $ toBuiltinData dat

-- | Pay value to the owner of PubKeyHash.
-- We use address to supply staking credential if we need it.
payToPubKeyAddress :: HasAddress pubKeyHash => pubKeyHash -> Value -> Tx
payToPubKeyAddress pkh val = toExtra $
  mempty
    { txOutputs = [TxOut (toAddress pkh) val Nothing]
    }

-- | Pay to the script.
-- We can use TypedValidator as argument and it will be checked that the datum is correct.
payToScript :: ToData (DatumType a) =>
  TypedValidator a -> DatumType a -> Value -> Tx
payToScript tv dat val = toExtra $
  mempty
    { txOutputs = [TxOut (toAddress tv) val (Just dh)]
    , txData = M.singleton dh datum
    }
  where
    dh = datumHash datum
    datum = Datum $ toBuiltinData dat

-- | Pay to the script.
-- We can use TypedValidator as argument and it will be checked that the datum is correct.
payToScriptAddress :: (HasAddress script, ToData datum) =>
  script -> datum -> Value -> Tx
payToScriptAddress script dat val = toExtra $
  mempty
    { txOutputs = [TxOut (toAddress script) val (Just dh)]
    , txData = M.singleton dh datum
    }
  where
    dh = datumHash datum
    datum = Datum $ toBuiltinData dat

-- | Pay fee for TX-submission
payFee :: Value -> Tx
payFee val = toExtra $
  mempty
    { txFee = val
    }

-- | Spend @TxOutRef@ that belongs to pub key (user).
spendPubKey :: TxOutRef -> Tx
spendPubKey ref = toExtra $
  mempty
    { txInputs = S.singleton $ TxIn ref (Just ConsumePublicKeyAddress)
    }

-- | Spend script input.
spendScript ::
  (ToData (DatumType a), ToData (RedeemerType a)) =>
  TypedValidator a ->
  TxOutRef ->
  RedeemerType a ->
  DatumType a ->
  Tx
spendScript tv ref red dat = toExtra $
  mempty
    { txInputs = S.singleton $ TxIn ref (Just $ ConsumeScriptAddress (validatorScript tv) (Redeemer $ toBuiltinData red) (Datum $ toBuiltinData dat))
    }

-- | Spend script input.
spendBox ::
  (ToData (DatumType a), ToData (RedeemerType a)) =>
  TypedValidator a ->
  RedeemerType a ->
  TxBox a ->
  Tx
spendBox tv red TxBox{..} =
  spendScript tv txBoxRef red txBoxDatum

-- | Specify that box is used as oracle (read-only). Spends value to itself and uses the same datum.
readOnlyBox :: (ToData (DatumType a), ToData (RedeemerType a))
  => TypedValidator a
  -> TxBox a
  -> RedeemerType a
  -> Tx
readOnlyBox tv box act = modifyBox tv box act id id

-- | Modifies the box. We specify how script box datum and value are updated.
modifyBox :: (ToData (DatumType a), ToData (RedeemerType a))
  => TypedValidator a
  -> TxBox a
  -> RedeemerType a
  -> (DatumType a -> DatumType a)
  -> (Value -> Value)
  -> Tx
modifyBox tv box act modDatum modValue = mconcat
  [ spendBox tv act box
  , payToScriptAddress box (modDatum $ txBoxDatum box) (modValue $ txOutValue $ txBoxOut box)
  ]

-- | Spend value for the user and also include change in the outputs.
userSpend :: UserSpend -> Tx
userSpend (UserSpend ins mChange) = toExtra $
  mempty
    { txInputs = ins
    , txOutputs = maybe [] pure mChange
    }

-- | Mints value. To use redeemer see function @addMintRedeemer@.
mintValue :: MintingPolicy -> Value -> Tx
mintValue policy val = toExtra $
  mempty
    { txMint = val
    , txMintScripts = S.singleton policy
    }

{- | Adds redeemr to the minting policy.
 Note that this should be done only as all mint values are specified.
 Otherwise it won't be attached and you should not attach any new mint values after it's done.

 So to mint value with redeemer first specify it with @mintValue@ and later on attach the redeemer:

 > mp = getmintinPolicy args
 > val = getMintVal args
 > red = getMintRedeemer args
 >
 > tx = addMintRedeemer mp red $ mconcat [mintValue mp val, ... other parts of tx... ]
-}
addMintRedeemer :: ToData redeemer => MintingPolicy -> redeemer -> Tx -> Tx
addMintRedeemer policy red = liftPlutusTx $ \tx ->
  maybe tx (setRedeemer tx . fst) $ L.find ((== policy) . snd) $ zip [0 ..] $ S.toList $ txMintScripts tx
  where
    setRedeemer tx ix =
      tx
        { txRedeemers = M.insert (RedeemerPtr Mint ix) (Redeemer $ toBuiltinData red) $ txRedeemers tx
        }

-- | Set validation time
validateIn :: POSIXTimeRange -> Tx -> Run Tx
validateIn times = updatePlutusTx $ \tx -> do
  slotCfg <- gets (bchConfigSlotConfig . bchConfig)
  pure $
    tx
      { txValidRange = Plutus.fmap (posixTimeToEnclosingSlot slotCfg) times
      }

----------------------------------------------------------------------
-- queries

-- | Typed txOut that contains decoded datum
data TxBox a = TxBox
  { txBoxRef   :: TxOutRef     -- ^ tx out reference
  , txBoxOut   :: TxOut        -- ^ tx out
  , txBoxDatum :: DatumType a  -- ^ datum
  }

deriving instance Show (DatumType a) => Show (TxBox a)
deriving instance Eq (DatumType a)   => Eq (TxBox a)

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

-- | Read UTXOs with datums.
boxAt :: (HasAddress addr, FromData (DatumType a)) => addr -> Run [TxBox a]
boxAt addr = do
  utxos <- utxoAt (toAddress addr)
  fmap catMaybes $ mapM (\(ref, tout) -> fmap (\dat -> TxBox ref tout dat) <$> datumAt ref) utxos

-- | Reads the Box for the script.
scriptBoxAt :: FromData (DatumType a) => TypedValidator a -> Run [TxBox a]
scriptBoxAt tv = boxAt (validatorAddress tv)

-- | It expects that Typed validator can have only one UTXO
-- which is NFT.
nftAt :: FromData (DatumType a) => TypedValidator a -> Run (TxBox a)
nftAt tv = head <$> scriptBoxAt tv

----------------------------------------------------------------------
-- time helpers

millis :: Integer -> POSIXTime
millis = POSIXTime

seconds :: Integer -> POSIXTime
seconds n = millis (1000 * n)

minutes :: Integer -> POSIXTime
minutes n = seconds (60 * n)

hours :: Integer -> POSIXTime
hours n = minutes (60 * n)

days :: Integer -> POSIXTime
days n = hours (24 * n)

weeks :: Integer -> POSIXTime
weeks n = days (7 * n)

----------------------------------------------------------------------
-- testing helpers

-- | Try to execute an action, and if it fails, restore to the current state
-- while preserving logs. If the action succeeds, logs an error as we expect
-- it to fail. Use 'mustFailWith' and 'mustFailWithBlock' to provide custom
-- error message or/and failure action name.
mustFail :: Run a -> Run ()
mustFail = mustFailWith  "Expected action to fail but it succeeds"

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
      infoLog <- gets bchInfo
      put st  { bchInfo = infoLog
             , mustFailLog = mkMustFailLog preFails postFails
             }
  where
    noNewErrors (fromLog -> a) (fromLog -> b) = length a == length b
    mkMustFailLog (unLog -> pre) (unLog -> post) =
      Log $ (second $ MustFailLog name) <$> Seq.drop (Seq.length pre) post

-- | Checks that script runs without errors and returns pretty printed failure
-- if something bad happens.
checkErrors :: Run (Maybe String)
checkErrors = do
  failures <- fromLog <$> getFails
  names <- gets bchNames
  pure $
    if null failures
      then Nothing
      else Just (init . unlines $ fmap (ppFailure names) failures)

-- | like 'testNoErrors' but prints out blockchain log for both
-- failing and successful tests. The recommended way to choose
-- between those two is using @tasty@ 'askOption'. To pull in
-- parameters use an 'Ingredient' built with 'includingOptions'.
testNoErrorsTrace :: Value -> BchConfig -> String -> Run a -> TestTree
testNoErrorsTrace funds cfg msg act =
    testCaseInfo msg $
      maybe (pure bchLog)
        assertFailure $ errors >>= \errs -> pure $ errs <> bchLog
  where
    (errors, bch) = runBch (act >> checkErrors) $ initBch cfg funds
    bchLog = "\n\nBlockchain log :\n----------------\n" <> ppBchEvent (bchNames bch) (getLog bch)

-- | Logs the blockchain state, i.e. balance sheet in the log
logBalanceSheet :: Run ()
logBalanceSheet =
  modify' $ \s -> s { bchInfo = appendLog (bchCurrentSlot s) (ppBalanceSheet s) (bchInfo s) }

testNoErrors :: Value -> BchConfig -> String -> Run a -> TestTree
testNoErrors funds cfg msg act =
   testCase msg $ maybe (pure ()) assertFailure $
    fst (runBch (act >> checkErrors) (initBch cfg funds))

-- | check transaction limits
testLimits :: Value -> BchConfig -> String -> (Log BchEvent -> Log BchEvent) -> Run a -> TestTree
testLimits initFunds cfg msg tfmLog act =
  testCase msg $ assertBool limitLog isOk
  where
    (isOk, bch) = runBch (act >> noErrors) (initBch (warnLimits cfg) initFunds)
    limitLog = ppLimitInfo (bchNames bch) $ tfmLog $ getLog bch

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
checkBalance (BalanceDiff diffs) act = do
  before <- mapM valueAt addrs
  res <- act
  after <- mapM valueAt addrs
  mapM_ (logError . show . vcat <=< mapM ppError) (check before after)
  pure res
  where
    ppError :: (Address, Value, Value) -> Run (Doc ann)
    ppError (addr, expected, got) = do
      names <- gets bchNames
      let addrName = maybe (pretty addr) pretty $ readAddressName names addr
      pure $ vcat
          [ "Balance error for:" <+> addrName
          , indent 2 $ vcat
              [ "Expected:" <+> ppBalanceWith names expected
              , "Got:" <+> ppBalanceWith names got
              ]
          ]

    addrs = M.keys diffs

    check :: [Value] -> [Value] -> Maybe [(Address, Value, Value)]
    check before after
      | null errs = Nothing
      | otherwise = Just errs
      where
        errs = catMaybes $ zipWith3 go addrs before after

        go addr a b
          | res Plutus.== dv = Nothing
          | otherwise        = Just (addr, dv, res)
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

withdrawTx :: Withdraw -> Tx
withdrawTx w = mempty { tx'extra = mempty { extra'withdraws = [w] } }

toRedeemer :: ToData red => red -> Redeemer
toRedeemer = Redeemer . toBuiltinData

withStakeScript :: ToData red => StakeValidator -> red -> Maybe (Redeemer, StakeValidator)
withStakeScript script red = Just (toRedeemer red, script)

-- | Add staking withdrawal based on pub key hash
withdrawStakeKey :: PubKeyHash -> Integer -> Tx
withdrawStakeKey key amount = withdrawTx $
  Withdraw (keyToStaking key) amount Nothing

-- | Add staking withdrawal based on script
withdrawStakeScript :: ToData redeemer
  => StakeValidator -> redeemer -> Integer -> Tx
withdrawStakeScript validator red amount = withdrawTx $
  Withdraw (scriptToStaking validator) amount (withStakeScript validator red)

certTx :: Certificate -> Tx
certTx cert = mempty { tx'extra = mempty { extra'certificates = [cert] } }

-- | Register staking credential by key
registerStakeKey :: PubKeyHash -> Tx
registerStakeKey pkh = certTx $
  Certificate (DCertDelegRegKey $ keyToStaking pkh) Nothing

-- | Register staking credential by stake validator
registerStakeScript :: ToData redeemer =>
  StakeValidator -> redeemer -> Tx
registerStakeScript script red = certTx $
  Certificate (DCertDelegRegKey $ scriptToStaking script) (withStakeScript script red)

-- | DeRegister staking credential by key
deregisterStakeKey :: PubKeyHash -> Tx
deregisterStakeKey pkh = certTx $
  Certificate (DCertDelegDeRegKey $ keyToStaking pkh) Nothing

-- | DeRegister staking credential by stake validator
deregisterStakeScript :: ToData redeemer =>
  StakeValidator -> redeemer -> Tx
deregisterStakeScript script red = certTx $
  Certificate (DCertDelegDeRegKey $ scriptToStaking script) (withStakeScript script red)

-- | Register staking pool
-- TODO: thois does not work on TX level.
-- Use insertPool as a workaround.
registerPool :: PoolId -> Tx
registerPool (PoolId pkh) = certTx $
  Certificate (DCertPoolRegister pkh pkh) Nothing

-- | Insert pool id to the list of stake pools
insertPool :: PoolId -> Run ()
insertPool pid = modify' $ \st ->
  st { bchStake = Stake.regPool pid $ bchStake st }

-- | delete pool from the list of stake pools
deletePool :: PoolId -> Run ()
deletePool pid = modify' $ \st ->
  st { bchStake = Stake.retirePool pid $ bchStake st }

-- | Retire staking pool
retirePool :: PoolId -> Tx
retirePool (PoolId pkh) = certTx $
  Certificate (DCertPoolRetire pkh 0) Nothing

-- | Delegates staking credential (specified by key) to pool
delegateStakeKey :: PubKeyHash -> PoolId -> Tx
delegateStakeKey stakeKey (PoolId poolKey) = certTx $
  Certificate (DCertDelegDelegate (keyToStaking stakeKey) poolKey) Nothing

-- | Delegates staking credential (specified by stakevalidator) to pool
delegateStakeScript :: ToData redeemer =>
  StakeValidator -> redeemer -> PoolId -> Tx
delegateStakeScript script red (PoolId poolKey) = certTx $
  Certificate (DCertDelegDelegate (scriptToStaking script) poolKey) (withStakeScript script red)
