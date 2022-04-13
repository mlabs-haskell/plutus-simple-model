module Plutus.Test.Model.Fork.TxExtra (
  -- * Plutus TX with extra fields
  Tx(..),
  Extra(..),
  Withdraw(..),
  toExtra,
  Certificate(..),
  getCertificateValidators,
  -- * utils
  updatePlutusTx,
  liftPlutusTx,
  keyToStaking,
  scriptToStaking,
) where

import Prelude
import Ledger qualified as P
import Plutus.V1.Ledger.Api
import qualified Data.Map.Strict as M

-- | Plutus TX with extra fields for Cardano TX fields that are missing
-- in native Plutus TX (staking and certificates).
data Tx = Tx
  { tx'extra  :: Extra
  , tx'plutus :: P.Tx
  }
  deriving (Show, Eq)

instance Semigroup Tx where
  (<>) (Tx a1 a2) (Tx b1 b2) = Tx (a1 <> b1) (a2 <> b2)

instance Monoid Tx where
  mempty = Tx mempty mempty

-- | Wrap TX to extra fields (empty fields are allocated)
toExtra :: P.Tx -> Tx
toExtra = Tx mempty

-- | Extra fields for Cardano TX
data Extra = Extra
  { extra'withdraws      :: [Withdraw]
  , extra'certificates   :: [Certificate]
  }
  deriving (Show, Eq)

instance Semigroup Extra where
  (<>) (Extra a1 a2) (Extra b1 b2) = Extra (a1 <> b1) (a2 <> b2)

instance Monoid Extra where
  mempty = Extra [] []

data Certificate = Certificate
  { certificate'dcert  :: DCert
  , certificate'script :: Maybe (Redeemer, StakeValidator)
  }
  deriving (Show, Eq)

getCertificateValidators :: [Certificate] -> M.Map StakingCredential (Redeemer, StakeValidator)
getCertificateValidators = foldMap go
  where
    go Certificate{..} = case certificate'dcert of
      DCertDelegRegKey stakeCred            -> fromCred stakeCred
      DCertDelegDeRegKey stakeCred          -> fromCred stakeCred
      DCertDelegDelegate stakeCred _poolKey -> fromCred stakeCred
      DCertPoolRegister _poolKey _poolVrf   -> mempty
      DCertPoolRetire _poolKey _epoch       -> mempty
      DCertGenesis                          -> mempty
      DCertMir                              -> mempty
      where
        fromCred cred = maybe mempty (M.singleton cred) certificate'script


-- | Stake withdrawal
data Withdraw = Withdraw
  { withdraw'credential :: StakingCredential                 -- ^ staking credential
  , withdraw'amount     :: Integer                           -- ^ amount of withdrawal in Lovelace
  , withdraw'script     :: Maybe (Redeemer, StakeValidator)  -- ^ Just in case of script withdrawal
  }
  deriving (Show, Eq)

updatePlutusTx :: Functor f => (P.Tx -> f P.Tx) -> Tx -> f Tx
updatePlutusTx f (Tx extra tx) = Tx extra <$> f tx

liftPlutusTx :: (P.Tx -> P.Tx) -> Tx -> Tx
liftPlutusTx f (Tx extra tx) = Tx extra (f tx)

keyToStaking :: PubKeyHash -> StakingCredential
keyToStaking = StakingHash . PubKeyCredential

scriptToStaking :: StakeValidator -> StakingCredential
scriptToStaking validator = StakingHash $ ScriptCredential vh
  where
    vh = P.validatorHash $ Validator $ getStakeValidator validator

