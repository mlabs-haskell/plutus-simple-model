import Plutus.Test.Model (defaultAlonzoBch)
import Suites.Plutus.Model.Script qualified as Script
import Suites.Plutus.Model.User qualified as User
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main = do
  let cfg = defaultAlonzoBch
  defaultMain $
    testGroup
      "Test Suites"
      [ User.tests cfg
      , Script.tests cfg
      ]
