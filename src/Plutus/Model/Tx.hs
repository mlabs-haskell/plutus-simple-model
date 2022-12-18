module Plutus.Model.Tx (
  Tx.Tx (
    Tx,
    txInputs,
    txCollateral,
    txReferenceInputs,
    txOutputs,
    txCollateralReturn,
    txTotalCollateral,
    txMint,
    txFee,
    txValidRange,
    txMintScripts,
    txSignatures,
    txRedeemers,
    txData,
    txScripts
  ),
  Tx.TxIn (
    TxIn,
    txInRef
  ),
) where

import Plutus.Model.Fork.Ledger.Tx qualified as Tx
