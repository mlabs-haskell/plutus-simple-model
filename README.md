Plutus simple model
====================================================

Unit test library for plutus with estimation of resource usage.

Library defines simple mock model of Blockchain to unit test plutus contracts 
and estimate usage of resources. What are benefits for this framework. It's:

* easy to use
* easy to think about
* fast
* can estimate usage of resources with real functions used on Cardano node.
* pure
* good for unit testing of onchain code


### Install

To add library to your project add it with niv:

```
niv add mlabs-haskell/plutus-simple-model -r <library-commit-hash>
```

And add it to caba.project:

```
-- library for unit tests of plutus scripts                                                                                       
source-repository-package
   type: git                                                                                                                         
   location: https://github.com/mlabs-haskell/plutus-simple-model
   tag: <same-library-commit-hash-as-for-niv>                                                                              
```

### Quick start guide

We can create simple blockchain data structure and update within context of State monad.
Update happens as a pure function and along TX-confirmation we have useful stats to estimate usage of resources.

To create blockchain we first need to specify blockchain config (`BchConfig`).
Config is specified by protocol parameters (`ProtocolParameters`) and era history (`EraHistory CardanoMode`).
They are cardano config data types. To avoid gory details it's safe to use predefined config
and load it with function:

```haskell
readDefaultBchConfig :: IO BchConfig
```

It reads config from files stored in the directory `data`. It's the only non-pure function that we have to use.

Once we have config available we can create initial state for blockchain with function:

```haskell
initBch :: BchConfig -> Value -> Blockchain
initBch config adminValue = ...
```

It creates blockchain that has only one UTXO that belongs to the admin user. The value is how many coins
we are going to give to the admin. Admin can distribute values to test users from admin's reserves.

The rest of the code happens within `Run` monad which is a thin wrapper on State over Blockchain under the hood.
We have scenarios of script usages as actions in the `Run` monad. When we are done we can get the result:


```haskell
runBch :: Run a -> Blockchain -> (a, Blockchain)
```

It just runs the state updates.

Let's create test users:

```haskell
-- alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ adaValue 1000
```

We can create new user and send values from admin to the user with the function:

```haskell
newUser :: Value -> Run PubKeyHash
```

Users are identified by their pub key hashes. Note that admin should have the value that we want to share
with new user. Otherwise we will get run-time exception.

Users can send values to each other with function:

```haskell
sendValue :: PubKeyHash -> Value -> PubKeyHash -> Run ()
```

Let's share some values:

```haskell
simpleSpend :: Run Bool
simpleSpend = do
  users <- setupUsers                -- create 3 users and assign each 1000 lovelaces
  let [u1, u2, u3] = users           -- give names for users
  sendValue u1 (adaValue 100) u2     -- send 100 from user 1 to user 2
  sendValue u2 (adaValue 100) u3     -- send 100 from user 2 to user 3
  isOk <- noErrors                   -- check that all TXs were accepted without errors
  vals <- mapM valueAt users         -- read user values
  pure $ and                         -- check test predicate
    [ isOk
     , vals == fmap adaValue [900, 1000, 1100]
    ]
```

This example shows how we can create simple tests with library.
We create three users and exchange the values between the users.
In the test we check that there are no errors (all TXs were accepted to blockchain) and
that users have expected values.

To check for TX errors we use:

```haskell
noErros :: Run Bool
```

Blockchain logs all failed transactions to te list `bchFails`. We check that this list is empty.

To read total value for the user we use:

```haskell
valueAt :: HasAddress addr => addr -> Run Value
```

Complete working example of user exchange can be found at test suites (see `Suites.Plutus.Model.User`)

So now we know how to send values to users. Let's learn how to work with scripts.

When we use function `sendValue` it creates TX and submits it to blockchain.
TX defines which UTXOs are used as inputs and what UTXOs we want to produce as outputs.
As a result (when TX is successful) we destroy input UTXOs and create output UTXOs.

UTXO can belong to the user (protected by the private PubKey) and can belong to the script (protected by the contract).
The function `spendValue` creates TX that uses sender's UTXO as input and signs it with sender's pub key
and creates receiver's UTXO as output sealed by receiver's pub key hash.

To post TXs to blockchain we have function:

```haskell
sendTx :: Tx -> Run (Either FailReason Stats)
```

It takes plutus Tx and tries to submit it. If it fails it returns he reason of failure.
If it succeeds it returns statistics of TX execution. It includes TX size if stored on Cardano
and execution units (execution steps and memory usage). Execution statistics is important to
know that our transaction is suitable for Cardano network. Cardano has certain limits that we should enforce
on our TXs.

Note that comparing to EmulatorTrace we don't need to use waitNSlots to force TX submission.
It's submited right away and slot counter is updated. If we want to submit block of TXs we
can use the function:

```haskell
sendBlock :: [Tx] -> Run (Either FailReason [Stats])
```

The function `sendTx` just submits a block with single TX in it. It's ok for most of the cases but
if we need to test acceptence of several TXs in the block we can use `sendBlock`.

Let's create Tx and submit it. We will use example of guess hash game script. See code of contracts in the test suite
(see `Suites.Plutus.Model.Onchain.Game`). Basic description is that anybody can create a Hash puzzle.
We can hash something and post the hash to blockchain alongside with the prize value. Anyone who can guess
the origin of hash can take the value.

For the game we need two TXs. One is to propose the puzzle and another one to solve it and get the prize.
Let's start with posting a puzzle. For that we need to give something out of our own value and
create UTXO that is protected by the Game contract.

Here is the definition:

```haskell
initGame :: PubKeyHash -> Value -> BuiltinByteString -> Run ()
initGame pkh prize answer = do                   -- arguments: user, value for the prize, answer for puzzle
  sp <- spend pkh prize                          -- read users UTXO that we should spend
  tx <- signTx pkh $ initGameTx sp prize answer  -- create TX and sign it with user's secret key
  void $ sendTx tx                               -- post TX to blockchain

-- pure function ot create TX
initGameTx :: Userspend -> Value -> BuiltinByteString -> Tx
```

Let's discuss the function bit by bit. To create TX we need to first determine the inputs.
Input is set of UTXO's that we'd like to spend. For that we use function

```haskell
spend :: PubKeyHash -> Value -> Run UserSpend
```

For a given user and value it returns a value of `UserSpend` which holds
a set of input UTXOs that cover the value that we want spend and also it has change to spend back to user.
In the UTXO model we can not split the input if it's bigger than we need. We have to destroy it
and create one UTXO that is given to someone else and another one that is spent back to user.
We can think about the latter UTXO as a change.

Note that it is going to produce run-time exception if there are not enough funds to spend.
To avoid run-time errors there is safer variant called `spend'`.
Also we can use safer alternative `withSpend`. It logs an error
if user has no funds to spend and continues execution which can
be helpful in some test cases:

```haskell
withSpend :: PubKeyHash -> Value -> (UserSpend -> Run ()) -> Run ()
```

When we know what inputs to spend we need to make a TX. We do it with function `initGameTx`. We will dicuss it soon.
After that we should sign TX with the key of the sender. We use function `signTx` for that.
As the last step we post TX and hope that it will execute fine (`sendTx`).

Let's discuss how to create TX:

```haskell
initGameTx :: UserSpend -> Value -> BuiltinByteString -> Tx
initGameTx usp val answer =
  mconcat
    [ userSpend usp
    , payToScript gameScript (GuessHash $ Plutus.sha2_256 answer) val
    ]
```

We create transaction by accumulation of monoidal parts. As plutus Tx is monoid it's convenient
to assemble it from tiny parts. For our task there are just two parts:

* for inputs and outputs for user spend
* pay prize to the script and form right datum for it.

We use function to make right part for spending the user inputs and sending change back to user:

```haskell
userSpend :: UserSpend -> Tx
```

To pay to script we use function:

```haskell
payToScript :: TypedValidator a -> DatumType a -> Value -> Tx
```

So it uses validator, datum for it (of proper type) and value to protect with the contract.
As simple as that.

Let's create another Tx to post solution to the puzzle. It seems to be more involved but don't be scary.
We will take it bit by bit:

```haskell
guess :: PubKeyHash -> BuiltinByteString -> Run Bool
guess pkh answer = do
  utxos <- utxoAt gameAddress            -- get game script UTXO
  let [(gameRef, gameOut)] = utxos       --   we know there is only one game UTXO
  mDat <- datumAt @GameDatum gameRef     -- read game's datum
  case mDat of                           -- if there is a datum
    Just dat -> do                       -- let's create TX and sign it
      tx <- signTx pkh $ guessTx pkh gameRef (txOutValue gameOut) dat answer
      isRight <$> sendTx tx              -- let's send TX to blockchain and find out weather it's ok.
    Nothing -> pure False
```

This function is a bit bigger because now we want to spend not only our funds but also the fund of the script.
For that we look up the script UTXO (`utxoAt`) and look up it's datum (`datumAt`) and when it all succeeds
we can form the right TX, sign it with our key and post it to blockchain.

Functions that query blockchain are often defined on addresses or `TxOutRef`'s:

```haskell
utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
datumAt :: TxOutRef -> Run (Maybe a)
```

We should query the datum separately because `TxOut` contains only hash of it.
Let's look at the pure function that creates TX. Again we assemble TX from monoidal parts:

```haskell
guessTx :: PubKeyHash -> TxOutRef -> Value -> GameDatum -> BuiltinByteString -> Tx
guessTx pkh gameRef gameVal dat answer =
  mconcat
    [ spendScript gameScript gameRef (Guess answer) dat
    , payToPubKey pkh gameVal
    ]
```

We do two things:

* spend script with right datum and redeemer
* pay the prize back to us

To spend script we use function:

```haskell
spendScript :: TypedValidator a -> TxOutRef -> RedeemerType a -> DatumType a -> Tx
```

We provide validator definition, reference to the UTXO of the script, and its redeemer and datum type.

The next thing is that we want to take the prize. For that we create output that holds the prize and
protected by our own pub key. We do it with the function:

```haskell
payToPubKey :: PubKeyHash -> Value -> Tx
```

### How to work with time

Note that every time we submit block successfully one slot passes.
By default one slot lasts for 1 second. Sometimes we want to check for TX that should
happen in the future or after some time passes.

For that we can use functions to make blockchain move forward:

```haskell
waitNSlots :: Slot -> Run ()
wait       :: POSIXTime -> Run ()
waitUntil  :: POSIXTime -> Run ()
```

`waitNSlots` waits in slots while `wait` and `waitUntil` wait in `POSIXTime` (counted in milliseconds).
Also we can query current time with functions:

```haskell
currentSlot :: Run Slot
currentTime :: Run POSIXTime
```

By default we always start at the beginning of UNIX epoch. Start of blockchain is set to 0 posix millis.
Closely related is function `validateIn`. It sets the valid range for TX:

```haskell
validateIn :: POSIXTimeRange -> Tx -> Run Tx
```

The result is wrapped in the Run-monad because we convert 
`POSIXTime` to `Slot`'s under the hood and conversion depends on blockchain config.

Note that if current time of blockchain is not included in the Tx it will be rejected.
So we can not submit TX that is going to be valid in the future. We rely on property
thhat all TXs are validated right away. It means  that current time should be included in valid range for TX.
By default it's `always`, it means "any time" and should work. 

Also note that because Plutus uses `POSIXTime` while the Cardano network uses Slots, the inherent
difference in precision between Slot and `POSIXTime` may cause unexpected validation failures.
For example, if we try to build a transaction using the `POSIXTimeRange` `(1100,3400)`, it will be converted to
`SlotRange` `(1,4)` to go through the Cardano network, and when it is converted back to Plutus, the POSIXRange
will have been extended to cover the whole slot range, becoming `(1000,4000)` and maybe trespassing
the allowed limits set by the validator.

POSIXTime is counted in milliseconds.
To count in human readable format we have convinient functions: `days`, `hours`, `minutes`, `seconds`:

```haskell
wait $ hours 4
```

That's it. you can find complete example at the test suite (see `Suites.Plutus.Model.Script.Test.Game`).
There are other useful function to dicuss. Look up the docs for the `Blockchain` and `Contract` modules.

### How to use custom coins

Often we need to use some custom coins to test the task.
To create new coin we usually need to create the minting policy script for it and
setup rules to mint the coins.

To make this task easy there are "fake" coins provided by the library.
To create fake coin we can use functions:

```haskell
testCoin :: FakeCoin
testCoin = FakeCoin "test-coin-A"

fakeValue :: FakeCoin -> Integer -> Value
fakeCoin  :: FakeCoin -> AssetClass
```

With those functions we can assign some fake coins to admin user on start up of the blockchain:

```haskell
testValue = fakeValue testCoin
bch = initBch config (adaValue 1000_000 <> testValue 1000)
```

In the blockchain code we can give those fake coins to users when we create them:

```haskell
u1 <- newUser (adaValue 100 <> testValue 10)
```

Note that when we send custom value from one user to another we also have to send some minimal ada value.
In Cardano every UTXO should have some ada in it. EmulatorTrace hides those details but
in this test framework it's exposed to the user.

So to send 5 test coins from one user to another add some ada to it:

```haskell
sendValue user1 (adaValue 1 <> testValue 5) user2
```

### Log custom erros

We can log our own errors with

```haskell
logErrors :: String -> Run ()
```

Errors are saved to log of errors. This way we can report our own errors based on conditions.
If values are wrong or certain NFT was not created etc.

Also we can log information with:

```haskell
logInfo :: String -> Run ()
```

It saves information to the log. the log of errors is unaffected.
We can read log messages with function:

```haskell
getLog :: Blockchain -> Log BchEvent
```

Where BchEvent is one of the events:

```haskell
data BchEvent
  = BchTx TxStat           -- ^ Sucessful TXs
  | BchInfo String         -- ^ Info messages
  | BchFail FailReason     -- ^ Errors
```

### How to check balances

There are useful functions to check not absolute balances but blaance transitions.
We have type `BalanceDiff` and we can construct it with function:

```haskell
owns  :: HasAddress addr => addr -> Value -> BalanceDiff
```

It states that address gained so many coins. We have useful function to check the move of values
from one address to another

```haskell
gives :: (HasAddress addrFrom, HasAddress addrTo) => addrFrom -> Value -> addrTo -> BalanceDiff
```

The type `BalanceDiff` is a monoid. So we can stack several transitions with `mappend` and `mconcat`.

To check the balances we use function:

```haskell
checkBalance :: BalanceDiff -> Run a -> Run a
```

It queries balances before application of an action and after application of the action and
reports any errors. If balances does not match to expected difference.

For example we can check that sendValue indeed transfers value from one user to another:

```haskell
checkBalance (gives u1 val u2) $ sendValue u1 val u2
```

If we want to check that user or script preserved the value we can set it up with `(owns user mempty)`.
Because TypedValidator has instance of the HasAddress we can also check payments to/from scripts:

```haskell
checkBalance (gives pkh prize gameScript) $ do { ... }
```

See more examples at tests for `Game` and `Counter` scripts.

### How to use in unit tests

For convenience there is a function `testNoErrors`:

```haskell
testNoErrors :: Value -> BchConfig -> String -> Run a -> TestTree
testNoErrors totalBchFunds bchConfig testMessage script = ...
```

It checks that given script runs without errors. Note that if we want to use custom
checks on run-time values we can query checks inside the script and log errors with `logError`.
By default it also checks for resource usage constraints.
If we want to check only logic but not resource usage we can use:

```haskell
skipLimits :: BchConfig -> BchConfig
```

Also there is function `warnLimits` that logs errors of resources usage
but does not fail TX for submission. So if logic is correct script will run but
errors of resources will be logged to error log.

### How to check TX resource usage

Sometimes we write too mcuh code in the validators and it starts to exceed execution limits.
TXs like this can not be executed on chain. To watch out for that we have special function:

```haskell
testLimits ::
   Value
   -> BchConfig
   -> String
   -> (Log BchEvent -> Log BchEvent)
   -> Run a
   -> TestTree
testLimits totalBchFunds bchConfig testMessage logFilter script
```

Let's break apart what it does. It runs blockchain with limit check config set to `WarnLimits`.
This way we proceed to execute TX onblockchain even if TX exceeds the limits but we save the
error on exceedance of the limits. When script was run if resource usage errors are encountered
they are logged to the user in easy to read way.

To see the logs even on sucessful run we can add fake error:

```haskell
(script >> logError "Show stats")
```

The results are shown in the percentage to the mainnet limit. We need to care
that all of them are below 100%. Also note that we'd better have some headroom 
and keep it not close to 100% because number of inputs in TX is unpredictable.
we can aggregate input values for the scripts from many UTXOs. So we'd better have 
the free room available for extra UTXOs.

Filter of the log can be usefl to filter out some non-related events. for example setup of the blockchain
users. We typically cn use it like this:

```haskell
good "Reward scripts" (filterSlot (> 4)) (Rewards.simpleRewardTestBy 1)
  where
    good = testLimits initFunds cfg
```

It's good to implement complete set of unit tests first and then add limits tests.
So that every transformation to optimise on resources is checked by ordinary unit tests.
On unit tests we can skip limit checks with `skipLimits :: BchConfig -> BchConfig`.

### Box - a typed TxOut

Often when we work with scripts we need to read `TxOut` to get datum hash to
read the datum next and after that we specify how datum is updated on TX.

Enter the Box - typed `TxOut`. The `Box` is `TxOut` augmented with typed datum. 
We can read Box for the script with the function:

```haskell
boxAt :: (HasAddress addr, FromData a) => addr -> Run [TxBox a]
```

It reads the typed box. We can use it like this: 

```haskell
gameBox <- head <$> boxAt @GameDatum gameScript
```

There is type safe variant that derives type of the datum from the type of the `TypedValidator`:

```haskell
scriptBoxAt :: FromData (DatumType a) => TypedValidator a -> Run [TxBox (DatumType a)]
```

Sometimes it's useful to read the box by NFT, since often scripts are identified by unque NFTs:

```haskell
nftAt :: FromData (DatumType a) => TypedValidator a -> Run (TxBox (DatumType a))
nftAt tv = ...
```

So let's look at the box:

```haskell
-- | Typed txOut that contains decoded datum
data TxBox a = TxBox
  { txBoxRef   :: TxOutRef   -- ^ tx out reference
  , txBoxOut   :: TxOut      -- ^ tx out
  , txBoxDatum :: a          -- ^ datum
  }

txBoxValue :: TxBox a -> Value
```

It has everything that TxOut has but also we have our typed datum.
There are functions that provide typical script usage. 

We can just spend boxes as scripts:

```haskell
spendBox ::
  (ToData (DatumType a), ToData (RedeemerType a)) =>
  TypedValidator a ->
  RedeemerType a ->
  TxBox (DatumType a) ->
  Tx
spendBox tv redeemer box
```

The most generic function is `modifyBox`:

```haskell
modifyBox :: (ToData (DatumType a), ToData (RedeemerType a))
  => TypedValidator a
  -> TxBox (DatumType a)
  -> RedeemerType a
  -> (DatumType a -> DatumType a)
  -> (Value -> Value)
  -> Tx
modifyBox tv box redeemer updateDatum updateValue
```

It specifies how we update the box datum and value. 
Also often we use boxes as oracles:

```haskell
readOnlyBox :: (ToData (DatumType a), ToData (RedeemerType a))
  => TypedValidator a
  -> TxBox (DatumType a)
  -> RedeemerType a
  -> Tx
```

It keeps the datum and value the same.

### How to pay to addresses with staking credentials

We can append the information on staking credential to
anything that is convertible to `Address` by `HasAddress` type class
with constructor `AppendStaking`. Also we have utility functions
`appendStakingPubKey` and `appendStakingScript` which
append `PubKeyHash` and `ValidatorHash` as staking credential.

For example we can append it to the `TypeValidator` of the script:

```haskell
payToScriptAddress (appendStakingPubKey stakingKey typedValidator) datum value
```

We use more generic version of `payToScript` which pays to 
anything convertible to address:

```haskell
payToScriptAddress :: (HasAddress script, ToData datum) =>
  script -> datum -> Value -> Tx
```

The same function exists to pay to pub key hash:

```haskell
payToPubKeyAddress :: HasAddress pubKeyHash => pubKeyHash -> Value -> Tx
```





