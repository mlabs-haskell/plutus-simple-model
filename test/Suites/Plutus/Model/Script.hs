module Suites.Plutus.Model.Script (
  tests,
) where

import Test.Tasty

import Plutus.Model
import Suites.Plutus.Model.Script.V1 qualified as V1

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Test scripts"
    [ V1.tests
    ]
