module Suites.Plutus.Model.Util (
  riderAda,
  setupUsers,
) where

import Control.Monad (replicateM)
import Plutus.Model
import Plutus.V1.Ledger.Api
import Prelude

-- alocate 3 users with 1000 lovelaces each
setupUsers :: Monad m => RunT m [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

riderAda :: Value
riderAda = ada (Lovelace 2)
