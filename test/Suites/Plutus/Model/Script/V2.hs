module Suites.Plutus.Model.Script.V2 (
  tests,
) where

import Test.Tasty

import Plutus.Test.Model
import Suites.Plutus.Model.Script.V2.Test.Game qualified as Game
import Suites.Plutus.Model.Script.V2.Test.GameRef qualified as GameRef
import Suites.Plutus.Model.Script.V2.Test.Lend qualified as Lend
import Suites.Plutus.Model.Script.V2.Test.Oracle qualified as Oracle

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Plutus V2 scripts"
    [ Game.tests cfg
    , GameRef.tests cfg
    , Lend.tests cfg
    , Oracle.tests cfg
    ]
