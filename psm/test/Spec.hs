import Prelude

import Test.Tasty (TestTree, defaultMain, testGroup)

import Plutus.Model (
  MockConfig,
  defaultAlonzo,
  defaultBabbageV1,
  defaultBabbageV2,
 )

import Suites.Plutarch as Plutarch
import Suites.Plutus.Model.Script.V1 qualified as Script.V1
import Suites.Plutus.Model.Script.V2 qualified as Script.V2
import Suites.Plutus.Model.User qualified as User

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Test Suites"
      [ plutusV1 "Alonzo" defaultAlonzo
      , plutusV1 "Babbage" defaultBabbageV1
      , plutusV2 "Babbage" defaultBabbageV2
      , Plutarch.tests
      ]

plutusV1 :: String -> MockConfig -> TestTree
plutusV1 name cfg =
  testGroup
    ("PlutusV1 " <> name)
    [ User.tests cfg
    , Script.V1.tests cfg
    ]

plutusV2 :: String -> MockConfig -> TestTree
plutusV2 name cfg =
  testGroup
    ("PlutusV2 " <> name)
    [ Script.V2.tests cfg
    ]
