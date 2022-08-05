# Creation of transactions

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

It takes extended Plutus Tx and tries to submit it. If it fails it returns the reason of failure.
If it succeeds it returns statistics of TX execution. It includes TX size if stored on Cardano
and execution units (execution steps and memory usage). Execution statistics is important to
know that our transaction is suitable for Cardano network. Cardano has certain limits that we should enforce
on our TXs.

Note that `Tx` is not a Plutus TX type. It contains Plutus `Tx` and extra info that we may need
to specify withdrawals of rewards and staking credentials. We need to use the custom type
because staking/credentials is not supported by Plutus TX out of the box. 

Note that comparing to EmulatorTrace we don't need to use waitNSlots to force TX submission.
It's submitted right away and slot counter is updated. If we want to submit block of TXs we
can use the function:

```haskell
sendBlock :: [Tx] -> Run (Either FailReason [Stats])
```

The function `sendTx` just submits a block with single TX in it. It's ok for most of the cases but
if we need to test acceptance of several TXs in the block we can use `sendBlock`.

There is a common pattern for testing to form single TX, sign it with a key and send to network. 
If we don't need to look at the stats we can use function. If it fails it logs the error.

```haskell
submitTx :: PubKeyHash -> Tx -> Run ()
submitTx pkh tx = void $ sendTx =<< signTx pkh tx
```

Let's create Tx and submit it. 


