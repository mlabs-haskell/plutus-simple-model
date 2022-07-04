module Suites.Plutus.Model.Script (
  tests,
) where

import Test.Tasty

import Plutus.Test.Model
import Suites.Plutus.Model.Script.Test.Counter qualified as Counter
import Suites.Plutus.Model.Script.Test.Game qualified as Game
-- import Suites.Plutus.Model.Script.Test.Nft qualified as Nft
-- import Suites.Plutus.Model.Script.Test.Safe qualified as Safe
-- import Suites.Plutus.Model.Script.Test.Staking qualified as Staking

tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Test scripts"
    [ Game.tests cfg
    -- test highlights bug with missing output datums, output datums are not inluded in Cardano TX,
    -- also see issue: <https://github.com/input-output-hk/plutus-apps/issues/139>
    -- TODO: fix that
   , Counter.tests cfg
--    , Safe.tests cfg
--    , Nft.tests cfg
--    , Staking.tests cfg
    ]
