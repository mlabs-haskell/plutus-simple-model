import Plutus.Model (MockConfig, defaultAlonzo, defaultBabbage)
import Suites.Plutus.Model.Script.V1 qualified as Script.V1
import Suites.Plutus.Model.Script.V2 qualified as Script.V2
import Suites.Plutus.Model.User qualified as User
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Test Suites"
      [ plutusV1 "Alonzo"  defaultAlonzo
      , plutusV1 "Babbage" defaultBabbage
      , plutusV2 "Babbage" defaultBabbage
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
