-- | Plutus TX
module Cardano.Simple.Ledger.Tx (
  Tx (..),
  txId,
  TxIn (..),
  TxInType (..),
) where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Data.Proxy
import Prelude

-- import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)

-- import PlutusLedgerApi.V2.Scripts

import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Ledger.Crypto qualified as C (StandardCrypto)
import Cardano.Ledger.Keys qualified as C
import Codec.CBOR.Write qualified as Write
import Codec.Serialise
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Tx
import PlutusTx.Lattice

import Cardano.Simple.Ledger.Scripts (Versioned (..))
import Cardano.Simple.Ledger.Slot
import Cardano.Simple.Plutus.Model.Ada (Ada)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts

-- | A transaction, including witnesses for its inputs.
data Tx = Tx
  { txInputs :: Set.Set TxIn
  -- ^ The inputs to this transaction.
  , txCollateral :: Set.Set TxIn
  -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
  , txReferenceInputs :: Set.Set TxIn
  -- ^ Reference inputs
  , txOutputs :: [TxOut]
  -- ^ The outputs of this transaction, ordered so they can be referenced by index.
  , txCollateralReturn :: Maybe TxOut
  -- ^ where to return the collateral
  , txTotalCollateral :: Maybe Ada
  -- ^ total collateral (see CIP-40)
  , txMint :: !Value
  -- ^ The 'Value' minted by this transaction.
  , txFee :: !Ada
  -- ^ The fee for this transaction.
  , txValidRange :: !SlotRange
  -- ^ The 'SlotRange' during which this transaction may be validated.
  , txMintScripts :: Set.Set (Versioned MintingPolicy)
  -- ^ The scripts that must be run to check minting conditions.
  , txSignatures :: Map.Map PubKeyHash (C.KeyPair 'C.Witness C.StandardCrypto)
  -- ^ Signatures of this transaction.
  , txRedeemers :: Redeemers
  -- ^ Redeemers of the minting scripts.
  , txData :: Map.Map DatumHash Datum
  -- ^ Datum objects recorded on this transaction.
  , txScripts :: Map.Map ScriptHash (Versioned Script)
  }
  deriving stock (Show, Generic)
  deriving anyclass ({-ToJSON, FromJSON, Serialise, -} NFData)

instance Semigroup Tx where
  tx1 <> tx2 =
    Tx
      { txInputs = txInputs tx1 <> txInputs tx2
      , txCollateral = txCollateral tx1 <> txCollateral tx2
      , txReferenceInputs = txReferenceInputs tx1 <> txReferenceInputs tx2
      , txOutputs = txOutputs tx1 <> txOutputs tx2
      , txCollateralReturn = txCollateralReturn tx1 <|> txCollateralReturn tx2
      , txMint = txMint tx1 <> txMint tx2
      , txTotalCollateral = txTotalCollateral tx1 <> txTotalCollateral tx2
      , txFee = txFee tx1 <> txFee tx2
      , txValidRange = txValidRange tx1 /\ txValidRange tx2
      , txMintScripts = txMintScripts tx1 <> txMintScripts tx2
      , txSignatures = txSignatures tx1 <> txSignatures tx2
      , txRedeemers = txRedeemers tx1 <> txRedeemers tx2
      , txData = txData tx1 <> txData tx2
      , txScripts = txScripts tx1 <> txScripts tx2
      }

instance Monoid Tx where
  mempty = Tx mempty mempty mempty mempty Nothing mempty mempty mempty top mempty mempty mempty mempty mempty

-- | Compute the id of a transaction.
txId :: Tx -> TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx =
  TxId $
    toBuiltin $
      digest (Proxy @SHA256) $
        digest
          (Proxy @SHA256)
          (Write.toStrictByteString $ encode $ show $ strip tx)

-- | A transaction without witnesses for its inputs.
data TxStripped = TxStripped
  { txStrippedInputs :: Set.Set TxOutRef
  -- ^ The inputs to this transaction, as transaction output references only.
  , txStrippedOutputs :: [TxOut]
  -- ^ The outputs of this translation.
  , txStrippedMint :: !Value
  -- ^ The 'Value' minted by this transaction.
  , txStrippedFee :: !Ada
  -- ^ The fee for this transaction.
  }
  deriving (Show, Eq, Generic)

strip :: Tx -> TxStripped
strip Tx {..} = TxStripped i txOutputs txMint txFee
  where
    i = Set.map txInRef txInputs

-- | A transaction input, consisting of a transaction output reference and an input type.
data TxIn = TxIn
  { txInRef :: !TxOutRef
  , txInType :: Maybe TxInType
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

data TxInType
  = -- TODO: these should all be hashes, with the validators and data segregated to the side

    -- | A transaction input that consumes a script address with the given validator, redeemer, and datum.
    ConsumeScriptAddress !(Maybe (Versioned Validator)) !Redeemer !Datum
  | -- | A transaction input that consumes a public key address.
    ConsumePublicKeyAddress
  | -- | Consume a simple script
    ConsumeSimpleScriptAddress
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
