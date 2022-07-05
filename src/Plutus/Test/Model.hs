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
module Plutus.Test.Model (
  module X,
) where

import Plutus.Test.Model.Blockchain as X
import Plutus.Test.Model.Contract   as X
import Plutus.Test.Model.Mint       as X
import Plutus.Test.Model.Pretty     as X
import Plutus.Test.Model.Validator  as X
