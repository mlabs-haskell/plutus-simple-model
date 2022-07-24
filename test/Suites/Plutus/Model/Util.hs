module Suites.Plutus.Model.Util (
  adaValue,
  riderAda,
  setupUsers,
) where

import Control.Monad (replicateM)
import Plutus.Test.Model
import Plutus.V1.Ledger.Api
import Prelude

adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken

-- alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser (adaValue 1000)

riderAda :: Value
riderAda = adaValue 2
