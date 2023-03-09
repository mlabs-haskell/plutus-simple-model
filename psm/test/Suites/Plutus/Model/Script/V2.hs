module Suites.Plutus.Model.Script.V2 (
  tests,
) where

import Test.Tasty

import Plutus.Model
import Suites.Plutus.Model.Script.V2.Test.Game qualified as Game
import Suites.Plutus.Model.Script.V2.Test.GameRef qualified as GameRef
import Suites.Plutus.Model.Script.V2.Test.Lend qualified as Lend
import Suites.Plutus.Model.Script.V2.Test.Oracle.Hashed qualified as Oracle.Hashed
import Suites.Plutus.Model.Script.V2.Test.Oracle.Inlined qualified as Oracle.Inlined

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Plutus V2 scripts"
    [ Game.tests cfg
    , GameRef.tests cfg
    , Lend.tests cfg
    , Oracle.Inlined.tests cfg
    , Oracle.Hashed.tests cfg
    ]
