module Plutus.Model.Stake(
  Stake(..),
  PoolId(..),
  Pool(..),
  reactDCert,
  DCertError(..),
  WithdrawError(..),
  checkDCert,
  withdrawStake,
  checkWithdrawStake,
  lookupReward,
  lookupStakes,
  rewardStake,
  regPool,
  retirePool,
) where

import Prelude
import Control.Applicative ((<|>))
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V

import Plutus.V1.Ledger.Api
import Plutus.Model.Mock.FailReason
import Plutus.Model.Fork.TxExtra (keyToStaking)

-- | Internal stake credentials state
data Stake = Stake
  { stake'pools      :: !(Map PoolId Pool)
  , stake'poolIds    :: !(Vector PoolId)
  , stake'stakes     :: !(Map StakingCredential Integer)
  , stake'nextReward :: !Int
  }

-- | Stake Pool key
newtype PoolId = PoolId { unPoolId :: PubKeyHash }
  deriving (Show, Eq, Ord)

data Pool = Pool
  { pool'stakes :: [StakingCredential]
  }

reactDCert :: DCert -> Stake -> Stake
reactDCert = \case
  DCertDelegRegKey cred       -> regStake cred
  DCertDelegDeRegKey cred     -> deregStake cred
  DCertDelegDelegate cred pkh -> delegateStake cred (PoolId pkh)
  -- TODO: pool parameters not supported
  DCertPoolRegister pkh _pvrf -> regPool (PoolId pkh)
  -- TODO: retire after so many epochs not supported
  DCertPoolRetire pkh _n      -> retirePool (PoolId pkh)
  DCertGenesis                -> error "DCertGenesis not supported"
  DCertMir                    -> error "DCertMir not supported"

checkDCert :: DCert -> Stake -> Maybe DCertError
checkDCert = \case
  DCertDelegRegKey cred       -> checkRegStake cred    `onFalse` RegStakeError cred
  DCertDelegDeRegKey cred     -> checkDeregStake cred  `onFalse` DeRegStakeError cred
  DCertDelegDelegate cred pkh -> checkDelegateStake cred (PoolId pkh) `onFalse` DelegateError cred pkh
  -- TODO: pool parameters not supported
  DCertPoolRegister pkh _pvrf -> checkRegPool (PoolId pkh) `onFalse` PoolRegError pkh
  -- TODO: retire after so many epochs not supported
  DCertPoolRetire pkh _n      -> checkRetirePool (PoolId pkh) `onFalse` PoolRetireError pkh
  DCertGenesis                -> const $ Just CertGenesisNotSupported
  DCertMir                    -> const $ Just CertMirNotSupported
  where
    onFalse answer err st = case answer st of
      True  -> Nothing
      False -> Just err

hasStakingCredential :: StakingCredential -> Stake -> Bool
hasStakingCredential cred st = M.member cred $ stake'stakes st

hasPool :: PoolId -> Stake -> Bool
hasPool pid st = V.elem pid $ stake'poolIds st

regPool :: PoolId -> Stake -> Stake
regPool pid st = st
  { stake'pools = M.insert pid pool $ stake'pools st
  , stake'poolIds = stake'poolIds st `V.snoc` pid
  }
  where
    pool = Pool [keyToStaking $ unPoolId pid]

checkRegPool :: PoolId -> Stake -> Bool
checkRegPool pid st = not $ hasPool pid st

retirePool :: PoolId -> Stake -> Stake
retirePool pid st = st
  { stake'pools = M.delete pid $ stake'pools st
  , stake'poolIds = V.filter (/= pid) $ stake'poolIds st
  }

checkRetirePool :: PoolId -> Stake -> Bool
checkRetirePool pid st = hasPool pid st

regStake :: StakingCredential -> Stake -> Stake
regStake cred st = st
  { stake'stakes = M.insert cred 0 $ stake'stakes st
  }

checkRegStake :: StakingCredential -> Stake -> Bool
checkRegStake cred st = not $ hasStakingCredential cred st

deregStake :: StakingCredential -> Stake -> Stake
deregStake cred st = st
  { stake'stakes = M.delete cred $ stake'stakes st
  }

checkDeregStake :: StakingCredential -> Stake -> Bool
checkDeregStake cred st = hasStakingCredential cred st

delegateStake :: StakingCredential -> PoolId -> Stake -> Stake
delegateStake cred pid st = st
  { stake'pools = M.adjust (\p -> p { pool'stakes = cred : pool'stakes p }) pid $ stake'pools st
  }

checkDelegateStake :: StakingCredential -> PoolId -> Stake -> Bool
checkDelegateStake cred pid st =
  hasStakingCredential cred st &&
  hasPool pid st

withdrawStake :: StakingCredential -> Stake -> Stake
withdrawStake cred st = st
  { stake'stakes = M.adjust (const 0) cred $ stake'stakes st
  }

checkWithdrawStake :: [PubKeyHash] -> StakingCredential -> Integer -> Stake -> Maybe WithdrawError
checkWithdrawStake signatures cred amount st =
  checkAmount <|> checkSign
  where
    checkAmount = case M.lookup cred $ stake'stakes st of
      Just reward | reward == amount && amount > 0 -> Nothing
      Just reward   -> Just $ WithdrawError cred amount reward
      Nothing       -> Just $ StakeNotRegistered cred

    checkSign = case cred of
      StakingHash (PubKeyCredential pkh) ->
        if elem pkh signatures
          then Nothing
          else Just (WithdrawNotSigned pkh)
      _ -> Nothing

lookupReward :: StakingCredential -> Stake -> Maybe Integer
lookupReward cred Stake{..} = M.lookup cred stake'stakes

lookupStakes :: PoolId -> Stake -> [StakingCredential]
lookupStakes pid Stake{..} = maybe [] pool'stakes $ M.lookup pid stake'pools

rewardStake :: Integer -> Stake -> Maybe Stake
rewardStake amount st =
  fmap update $ M.lookup pid $ stake'pools st
  where
    len     = V.length (stake'poolIds st)
    current = stake'nextReward st `mod` len
    next    = succ current `mod` len
    pid     = stake'poolIds st V.! current

    update p = st
      { stake'nextReward = next
      , stake'stakes     = L.foldl' addReward (stake'stakes st) rewards
      }
      where
        stakes = pool'stakes p
        (d, m) = amount `divMod` fromIntegral (length stakes)
        rewards = (keyToStaking $ unPoolId pid, m) : fmap (, d) stakes

        addReward res (cred, val) = M.adjust (+ val) cred res

