module Plutus.Test.Model.Blockchain.Log(
  Log(..),
  appendLog,
  nullLog,
  fromLog,
  fromGroupLog,
  MustFailLog(..),
  FailReason(..),
  LimitOverflow(..),
  BchEvent(..),
  silentLog,
  failLog,
  filterSlot,
) where

import Prelude
import Data.Foldable
import Data.Function (on)
import Data.List qualified as L
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Plutus.V1.Ledger.Api
import Plutus.Test.Model.Stake
import Plutus.Test.Model.Blockchain.Stat
import Plutus.Test.Model.Fork.Ledger.Slot

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

-- | Wrapper for error logs, produced in the paths of execution protected by
-- 'mustFail' combinator.
data MustFailLog = MustFailLog String FailReason

-- | Fail reasons.
data FailReason
  = -- | use with given pub key hash is not found. User was not registered with @newUser@ or @newUserWith@.
    NoUser PubKeyHash
  | -- | not enough funds for the user.
    NotEnoughFunds PubKeyHash Value
  | -- | TX is not balanced. Sum of inputs does not equal to sum of outputs.
    NotBalancedTx
  | -- | no utxo on the address
    FailToReadUtxo
  | -- | failed to convert plutus TX to cardano TX. TX is malformed.
    FailToCardano String
  | -- | invalid range. TX is submitted with current slot not in valid range
    TxInvalidRange Slot SlotRange
    -- | invalid reward for staking credential, expected and actual values for stake at the moment of reward
  | TxInvalidWithdraw WithdrawError
    -- | Certificate errors
  | TxInvalidCertificate DCertError
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

-- | Blockchain events to log.
data BchEvent
  -- | Sucessful TXs
  = BchTx
    { bchTx'name   :: Maybe String -- ^ Optional tx's name
    , bchTx'txStat :: TxStat       -- ^ Tx and stat
    }
  | BchInfo String             -- ^ Info messages
  | BchFail FailReason         -- ^ Errors
  | BchMustFailLog MustFailLog -- ^ Expected errors, see 'mustFail'

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
      BchTx _ _ -> True
      _         -> False

-- | filter by slot. Can be useful to filter out unnecessary info.
filterSlot :: (Slot -> Bool) -> Log a -> Log a
filterSlot f (Log xs) = Log (Seq.filter (f . fst) xs)


