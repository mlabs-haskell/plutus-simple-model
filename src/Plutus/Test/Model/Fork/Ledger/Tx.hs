-- | Plutus TX
module Plutus.Test.Model.Fork.Ledger.Tx(
  Tx(..),
  txId,
) where

import Prelude
import Data.Proxy
import Control.DeepSeq (NFData)
-- import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
-- import Plutus.V2.Ledger.Scripts
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Api
import PlutusTx.Lattice
import Cardano.Crypto.Hash (SHA256, digest)
import Codec.CBOR.Write qualified as Write
import Codec.Serialise
import Cardano.Ledger.Keys qualified as C
import Cardano.Ledger.Crypto qualified as C (StandardCrypto)

import Plutus.Test.Model.Fork.Ledger.Ada (Ada)
import Plutus.Test.Model.Fork.Ledger.Slot

-- | A transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs      :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txCollateral  :: Set.Set TxIn,
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txReferenceInputs :: Set.Set TxIn,
    -- ^ Reference inputs
    txOutputs     :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint        :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee         :: !Ada,
    -- ^ The fee for this transaction.
    txValidRange  :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintScripts :: Set.Set MintingPolicy,
    -- ^ The scripts that must be run to check minting conditions.
    txSignatures  :: Map.Map PubKeyHash (C.KeyPair 'C.Witness C.StandardCrypto),
    -- ^ Signatures of this transaction.
    txRedeemers   :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    txData        :: Map.Map DatumHash Datum
    -- ^ Datum objects recorded on this transaction.
    } deriving stock (Show, Generic)
      deriving anyclass ({-ToJSON, FromJSON, Serialise, -} NFData)


instance Semigroup Tx where
    tx1 <> tx2 = Tx {
        txInputs = txInputs tx1 <> txInputs tx2,
        txCollateral = txCollateral tx1 <> txCollateral tx2,
        txReferenceInputs = txReferenceInputs tx1 <> txReferenceInputs tx2,
        txOutputs = txOutputs tx1 <> txOutputs tx2,
        txMint = txMint tx1 <> txMint tx2,
        txFee = txFee tx1 <> txFee tx2,
        txValidRange = txValidRange tx1 /\ txValidRange tx2,
        txMintScripts = txMintScripts tx1 <> txMintScripts tx2,
        txSignatures = txSignatures tx1 <> txSignatures tx2,
        txRedeemers = txRedeemers tx1 <> txRedeemers tx2,
        txData = txData tx1 <> txData tx2
        }

instance Monoid Tx where
    mempty = Tx mempty mempty mempty mempty mempty mempty top mempty mempty mempty mempty

-- | Compute the id of a transaction.
txId :: Tx -> TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx = TxId $ toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ show $ strip tx)


-- | A transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs  :: Set.Set TxOutRef,
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedOutputs :: [TxOut],
    -- ^ The outputs of this transation.
    txStrippedMint    :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txStrippedFee     :: !Ada
    -- ^ The fee for this transaction.
    } deriving (Show, Eq, Generic)

strip :: Tx -> TxStripped
strip Tx{..} = TxStripped i txOutputs txMint txFee where
    i = Set.map txInRef txInputs

