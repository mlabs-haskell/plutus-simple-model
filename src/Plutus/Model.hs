{- | Simple mock model of Blockchain to unit test plutus contracts and estimate usage of resources.

  What are benefits for this framework. It's:

  * easy to use
  * easy to think about
  * fast
  * can estimate usage of resources with real functions used on Cardano node.
  * pure
  * good for unit testing of onchain code

  For tutorial and docs see README for the plutus-simple-model repo.
-}
module Plutus.Model (
  module X,
) where

import Plutus.Model.Ada as X
import Plutus.Model.Contract as X
import Plutus.Model.Mint as X
import Plutus.Model.Mock as X
import Plutus.Model.Pretty as X
import Plutus.Model.Validator as X
