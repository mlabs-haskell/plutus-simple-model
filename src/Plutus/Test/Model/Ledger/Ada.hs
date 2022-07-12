module Plutus.Test.Model.Ledger.Ada(
      Ada (..)
    , getAda
    , adaSymbol
    , adaToken
    -- * Constructors
    , fromValue
    , toValue
    , lovelaceOf
    , adaOf
    , lovelaceValueOf
    , adaValueOf
    , lovelaceValue
    , adaValue
    -- * Num operations
    , divide
    -- * Etc.
    , isZero
    ) where

import PlutusTx.Prelude (Integer, MultiplicativeSemigroup(..))
import Plutus.V1.Ledger.Value (Value)
import Plutus.Test.Model.Fork.Ledger.Ada

adaValue :: Integer -> Value
adaValue n = toValue (Lovelace (n * 1_000_000))

lovelaceValue :: Integer -> Value
lovelaceValue n = toValue (Lovelace n)

