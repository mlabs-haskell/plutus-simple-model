module Suites.Plutus.Model.Util (
  riderAda,
  setupUsers,
) where

import Control.Monad (replicateM)
import Plutus.Model
import PlutusLedgerApi.V2
import Prelude

-- alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

riderAda :: Value
riderAda = ada (Lovelace 2)
