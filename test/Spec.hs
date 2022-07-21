import Plutus.Test.Model (BchConfig, defaultAlonzo, defaultBabbage)
import Suites.Plutus.Model.Script qualified as Script
import Suites.Plutus.Model.User qualified as User
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Test Suites"
      [ plutusV1 "Alonzo"  defaultAlonzo
      , plutusV1 "Babbage" defaultBabbage
      ]

plutusV1 :: String -> BchConfig -> TestTree
plutusV1 name cfg =
    testGroup
      ("PlutusV1 " <> name)
      [ User.tests cfg
      , Script.tests cfg
      ]
