module Plutus.Model.Fork.TxExtra (
  -- * Plutus TX with extra fields
  Tx (..),
  Extra (..),
  Withdraw (..),
  toExtra,
  Mint (..),
  Certificate (..),
  getCertificateValidators,

  -- * utils
  updatePlutusTx,
  liftPlutusTx,
  keyToStaking,
  scriptToStaking,
  processMints,
) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Set qualified as S
import Plutus.Model.Fork.Ledger.Scripts (Versioned (..), scriptCurrencySymbol)
import Plutus.Model.Fork.Ledger.Scripts qualified as P
import Plutus.Model.Fork.Ledger.Tx qualified as P
import Plutus.Model.Fork.PlutusLedgerApi.V1.Scripts
import Plutus.Model.Mock.FailReason
import PlutusLedgerApi.V1.Tx qualified as P
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2
import PlutusTx.Prelude qualified as PlutusTx
import Prelude

{- | Plutus TX with extra fields for Cardano TX fields that are missing
 in native Plutus TX (staking and certificates).
-}
data Tx = Tx
  { tx'extra :: Extra
  , tx'plutus :: P.Tx
  }
  deriving (Show)

instance Semigroup Tx where
  (<>) (Tx a1 a2) (Tx b1 b2) = Tx (a1 <> b1) (a2 <> b2)

instance Monoid Tx where
  mempty = Tx mempty mempty

-- | Wrap TX to extra fields (empty fields are allocated)
toExtra :: P.Tx -> Tx
toExtra = Tx mempty

-- | Extra fields for Cardano TX
data Extra = Extra
  { extra'mints :: [Mint]
  , extra'withdraws :: [Withdraw]
  , extra'certificates :: [Certificate]
  }
  deriving (Show, Eq)

instance Semigroup Extra where
  (<>) (Extra a1 a2 a3) (Extra b1 b2 b3) = Extra (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid Extra where
  mempty = Extra [] [] []

data Mint = Mint
  { mint'value :: Value
  , mint'redeemer :: Redeemer
  , mint'policy :: Versioned MintingPolicy
  }
  deriving (Show, Eq)

-- | Converts mints from TxExtra to Plutus.Tx
processMints :: Tx -> Either FailReason Tx
processMints tx =
  case getMissingMints =<< mints of
    [] -> Right $ tx {tx'plutus = appendMints mints $ tx'plutus tx}
    missingSymbols -> Left $ NoMintingPolicy missingSymbols
  where
    mints = extra'mints $ tx'extra tx

getMissingMints :: Mint -> [CurrencySymbol]
getMissingMints Mint {..} = filter (PlutusTx./= policySymbol) symbols
  where
    symbols = (\(cs, _, _) -> cs) <$> Value.flattenValue mint'value
    policySymbol = scriptCurrencySymbol mint'policy

appendMints :: [Mint] -> P.Tx -> P.Tx
appendMints mints ptx =
  L.foldl' addMintRedeemer ptxWithMints mints
  where
    toMintTx (Mint value _redeemer policy) =
      mempty {P.txMint = value, P.txMintScripts = S.singleton policy}

    addMintRedeemer resTx (Mint _value redeemer policy) =
      maybe resTx (setRedeemer resTx . fst) $ L.find ((== policy) . snd) indexedMints
      where
        setRedeemer tx ix =
          tx
            { P.txRedeemers = M.insert (P.RedeemerPtr P.Mint ix) redeemer $ P.txRedeemers tx
            }

    ptxWithMints = foldMap toMintTx mints <> ptx
    indexedMints = zip [0 ..] $ S.toList $ P.txMintScripts ptxWithMints

data Certificate = Certificate
  { certificate'dcert :: DCert
  , certificate'script :: Maybe (Redeemer, Versioned StakeValidator)
  }
  deriving (Show, Eq)

getCertificateValidators :: [Certificate] -> M.Map StakingCredential (Redeemer, Versioned StakeValidator)
getCertificateValidators = foldMap go
  where
    go Certificate {..} = case certificate'dcert of
      DCertDelegRegKey stakeCred -> fromCred stakeCred
      DCertDelegDeRegKey stakeCred -> fromCred stakeCred
      DCertDelegDelegate stakeCred _poolKey -> fromCred stakeCred
      DCertPoolRegister _poolKey _poolVrf -> mempty
      DCertPoolRetire _poolKey _epoch -> mempty
      DCertGenesis -> mempty
      DCertMir -> mempty
      where
        fromCred cred = maybe mempty (M.singleton cred) certificate'script

-- | Stake withdrawal
data Withdraw = Withdraw
  { withdraw'credential :: StakingCredential
  -- ^ staking credential
  , withdraw'amount :: Integer
  -- ^ amount of withdrawal in Lovelace
  , withdraw'script :: Maybe (Redeemer, Versioned StakeValidator)
  -- ^ Just in case of script withdrawal
  }
  deriving (Show, Eq)

updatePlutusTx :: Functor f => (P.Tx -> f P.Tx) -> Tx -> f Tx
updatePlutusTx f (Tx extra tx) = Tx extra <$> f tx

liftPlutusTx :: (P.Tx -> P.Tx) -> Tx -> Tx
liftPlutusTx f (Tx extra tx) = Tx extra (f tx)

keyToStaking :: PubKeyHash -> StakingCredential
keyToStaking = StakingHash . PubKeyCredential

scriptToStaking :: Versioned StakeValidator -> StakingCredential
scriptToStaking validator = StakingHash $ ScriptCredential vh
  where
    vh = P.validatorHash $ fmap (Validator . getStakeValidator) validator
