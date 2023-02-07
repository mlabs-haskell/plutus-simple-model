-- | Errors for TX build and submit
module Plutus.Model.Mock.FailReason (
  FailReason (..),
  DCertError (..),
  WithdrawError (..),
  LimitOverflow (..),
) where

import Cardano.Simple.Ledger.Slot
import Plutus.Model.Mock.Percent
import PlutusLedgerApi.V2
import Prelude

-- | Fail reasons.
data FailReason
  = -- | use with given pub key hash is not found. User was not registered with @newUser@ or @newUserWith@.
    NoUser PubKeyHash
  | -- | not enough funds for the user.
    NotEnoughFunds PubKeyHash Value
  | -- | TX is not balanced. Sum of inputs does not equal to sum of outputs.
    NotBalancedTx Value
  | -- | no utxo on the address (argument is the balance difference)
    FailToReadUtxo
  | -- | failed to convert plutus TX to cardano TX. TX is malformed.
    FailToCardano String
  | -- | invalid range. TX is submitted with current slot not in valid range
    TxInvalidRange Slot SlotRange
  | -- | invalid reward for staking credential, expected and actual values for stake at the moment of reward
    TxInvalidWithdraw WithdrawError
  | -- | Certificate errors
    TxInvalidCertificate DCertError
  | TxLimitError [LimitOverflow] StatPercent
  | -- | Any error (can be useful to report logic errors on testing)
    GenericFail String
  | -- | Missing minting policy script for the currency symbol
    NoMintingPolicy [CurrencySymbol]
  deriving (Show)

data DCertError
  = RegStakeError StakingCredential
  | DeRegStakeError StakingCredential
  | DelegateError StakingCredential PubKeyHash
  | PoolRegError PubKeyHash
  | PoolRetireError PubKeyHash
  | CertGenesisNotSupported
  | CertMirNotSupported
  deriving (Show, Eq)

data WithdrawError
  = WithdrawError StakingCredential Integer Integer
  | WithdrawNotSigned PubKeyHash
  | StakeNotRegistered StakingCredential
  deriving (Show, Eq)

-- | Encodes overflow of the TX-resources
data LimitOverflow
  = -- | by how many bytes we exceed the limit
    TxSizeError !Integer !Percent
  | -- | how many mem units exceeded
    ExMemError !Integer !Percent
  | -- | how many steps executions exceeded
    ExStepError !Integer !Percent
  deriving (Show, Eq)
