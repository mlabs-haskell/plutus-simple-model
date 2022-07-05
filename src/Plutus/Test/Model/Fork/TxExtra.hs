module Plutus.Test.Model.Fork.TxExtra (
  -- * Plutus TX with extra fields
  Tx(..),
  Extra(..),
  Withdraw(..),
  toExtra,
  Mint(..),
  Certificate(..),
  getCertificateValidators,
  -- * utils
  updatePlutusTx,
  liftPlutusTx,
  keyToStaking,
  scriptToStaking,
  processMints,
) where

import Data.Monoid
import Data.List qualified as L
import Data.Set qualified as S
import Prelude
import Plutus.Test.Model.Fork.Ledger.Tx qualified as P
import Plutus.V1.Ledger.Api
import qualified Data.Map.Strict as M
import Plutus.Test.Model.Fork.Ledger.Scripts qualified as P
import Plutus.V1.Ledger.Tx qualified as P

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
  { extra'mints          :: [Mint]
  , extra'withdraws      :: [Withdraw]
  , extra'certificates   :: [Certificate]
  }
  deriving (Show, Eq)

instance Semigroup Extra where
  (<>) (Extra a1 a2 a3) (Extra b1 b2 b3) = Extra (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid Extra where
  mempty = Extra [] [] []

data Mint = Mint
  { mint'value    :: Value
  , mint'redeemer :: Redeemer
  , mint'policy   :: MintingPolicy
  }
  deriving (Show, Eq)

-- | Converts mints from TxExtra to Plutus.Tx
processMints :: Tx -> Tx
processMints tx = tx { tx'plutus = appendMints mints $ tx'plutus tx }
  where
    mints = extra'mints $ tx'extra tx

appendMints :: [Mint] -> P.Tx -> P.Tx
appendMints mints ptx =
  L.foldl' addMintRedeemer (foldMap toMintTx mints <> ptx) mints
  where
    toMintTx (Mint value _redeemer policy) =
      mempty { P.txMint = value, P.txMintScripts = S.singleton policy }

    addMintRedeemer resTx (Mint _value redeemer policy) =
      maybe resTx (setRedeemer resTx . fst) $ L.find ((== policy) . snd) $ zip [0 ..] $ S.toList $ P.txMintScripts resTx
      where
        setRedeemer tx ix =
          tx
            { P.txRedeemers = M.insert (P.RedeemerPtr P.Mint ix) redeemer $ P.txRedeemers tx
            }

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


