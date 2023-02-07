module Plutus.Model.Mock.Log (
  Log (..),
  appendLog,
  nullLog,
  fromLog,
  fromGroupLog,
  MustFailLog (..),
  LimitOverflow (..),
  MockEvent (..),
  silentLog,
  failLog,
  filterSlot,
) where

import Cardano.Simple.Ledger.Slot
import Data.Foldable
import Data.Function (on)
import Data.List qualified as L
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Plutus.Model.Mock.FailReason
import Plutus.Model.Mock.Stat
import Prelude

-- | Log of Slot-timestamped events
newtype Log a = Log {unLog :: Seq (Slot, a)}
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

-- | Insert event to log
appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

-- | Empty log
nullLog :: Log a -> Bool
nullLog (Log a) = Seq.null a

-- | Convert log to plain list
fromLog :: Log a -> [(Slot, a)]
fromLog (Log s) = toList s

fromGroupLog :: Log a -> [(Slot, [a])]
fromGroupLog = fmap toGroup . L.groupBy ((==) `on` fst) . fromLog
  where
    toGroup ((a, b) : rest) = (a, b : fmap snd rest)
    toGroup [] = error "toGroup: Empty list"

instance Monoid (Log a) where
  mempty = Log Seq.empty

{- | Wrapper for error logs, produced in the paths of execution protected by
 'mustFail' combinator.
-}
data MustFailLog = MustFailLog String FailReason

-- | Blockchain events to log.
data MockEvent
  = -- | Sucessful TXs
    MockTx
      { mockTx'name :: Maybe String
      -- ^ Optional tx's name
      , mockTx'txStat :: TxStat
      -- ^ Tx and stat
      }
  | -- | Info messages
    MockInfo String
  | -- | Errors
    MockFail FailReason
  | -- | Expected errors, see 'mustFail'
    MockMustFailLog MustFailLog

-- | Skip all info messages
silentLog :: Log MockEvent -> Log MockEvent
silentLog (Log xs) = Log $ Seq.filter (not . isInfo . snd) xs
  where
    isInfo = \case
      MockInfo _ -> True
      _ -> False

-- | Skip successful TXs
failLog :: Log MockEvent -> Log MockEvent
failLog (Log xs) = Log $ Seq.filter (not . isTx . snd) xs
  where
    isTx = \case
      MockTx _ _ -> True
      _ -> False

-- | filter by slot. Can be useful to filter out unnecessary info.
filterSlot :: (Slot -> Bool) -> Log a -> Log a
filterSlot f (Log xs) = Log (Seq.filter (f . fst) xs)
