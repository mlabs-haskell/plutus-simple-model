module Suites.Plutus.Model.Script.V1 (
  tests,
) where

import Test.Tasty

import Plutus.Model
import Suites.Plutus.Model.Script.V1.Test.Counter qualified as Counter
import Suites.Plutus.Model.Script.V1.Test.Game qualified as Game
import Suites.Plutus.Model.Script.V1.Test.Nft qualified as Nft
import Suites.Plutus.Model.Script.V1.Test.Safe qualified as Safe
import Suites.Plutus.Model.Script.V1.Test.Staking qualified as Staking

tests :: MockConfig -> TestTree
tests cfg =
  testGroup
    "Plutus V1 scripts"
    [ Game.tests cfg
    , -- test highlights bug with missing output datums, output datums are not inluded in Cardano TX,
      -- also see issue: <https://github.com/input-output-hk/plutus-apps/issues/139>
      -- TODO: fix that
      Counter.tests cfg
    , Nft.tests cfg
    , Safe.tests cfg
    , Staking.tests cfg
    ]
