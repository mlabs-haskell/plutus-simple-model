module Suites.Plutus.Model.Script.V2 (
  tests,
) where

import Test.Tasty

import Plutus.Test.Model
import Suites.Plutus.Model.Script.V2.Test.Game qualified as Game

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Plutus V2 scripts"
    [ Game.tests cfg
    ]
