-- | Functions for working with 'Ada'.
module Plutus.Model.Ada(
      Ada (..),
      adaOf,
      ada,
      adaValue,
      isZero,
      asAda,
      adaToLovelace,
      adaSymbol,
      adaToken,
      divideAda,
      onlyAda,
    ) where

import Prelude qualified as Haskell

import Control.DeepSeq (NFData)

import Codec.Serialise.Class (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Tagged
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value (Value, adaSymbol, adaToken)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude
import Prettyprinter (Pretty)

-- | ADA, the special currency on the Cardano blockchain. The unit of Ada is Lovelace, and
--   1M Lovelace is one Ada.
newtype Ada = Lovelace { getLovelace :: Integer }
    deriving (Haskell.Enum)
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Eq, Ord, Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, MultiplicativeSemigroup, MultiplicativeMonoid, Haskell.Integral, Haskell.Real, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, NFData)
    deriving Pretty via (Tagged "Lovelace:" Integer)

instance Haskell.Semigroup Ada where
    Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Semigroup Ada where
    Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Haskell.Monoid Ada where
    mempty = Lovelace 0

instance Monoid Ada where
    mempty = Lovelace 0

instance Module Integer Ada where
  scale n (Lovelace v) = Lovelace (n * v)

makeLift ''Ada

{-# INLINABLE adaOf #-}
adaOf :: Value -> Ada
adaOf v = Lovelace (Value.valueOf v adaSymbol adaToken)

{-# INLINABLE ada #-}
ada :: Ada -> Value
ada (Lovelace amount) = Value.singleton adaSymbol adaToken amount

{-# INLINABLE adaValue #-}
adaValue :: Integer -> Value
adaValue = ada . Lovelace

{-# INLINABLE isZero #-}
-- | Check whether an 'Ada' value is zero.
isZero :: Ada -> Bool
isZero (Lovelace i) = i == 0

{-# INLINABLE asAda #-}
-- | Counts in Ada's not in lovelaces
asAda :: Integer -> Ada
asAda = Lovelace . adaToLovelace

{-# INLINABLE adaToLovelace #-}
adaToLovelace :: Integer -> Integer
adaToLovelace n = 1_000_000 * n

{-# INLINABLE divideAda #-}
-- | Divide one 'Ada' value by another.
divideAda :: Ada -> Ada -> Ada
divideAda (Lovelace a) (Lovelace b) = Lovelace (divide a b)

{-# INLINABLE onlyAda #-}
onlyAda :: Value -> Value
onlyAda v = ada (adaOf v)



