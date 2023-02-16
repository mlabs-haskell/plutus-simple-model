{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Suites.Plutus.Model.Script.V2.Onchain.PMint (
  mkVerifyAuthUntyped,
  mkVerifyAuth,
  MintParams (..),
) where

import GHC.Generics qualified as GHC
import Plutarch.Api.V2 (PMintingPolicy, PPubKeyHash, PScriptContext (..), PScriptPurpose (..))
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  PubKeyHash,
 )
import PlutusTx (makeIsDataIndexed)
import Prelude

newtype PMintParams (s :: S)
  = PMintParams
      ( Term
          s
          ( PDataRecord
              '[ "adminKey" ':= PPubKeyHash
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMintParams where
  type DPTStrat _ = PlutusTypeData

mkVerifyAuthUntyped ::
  ClosedTerm
    (PData :--> PMintingPolicy)
mkVerifyAuthUntyped = plam $ \params _ ctx -> P.do
  mkVerifyAuth
    # punsafeCoerce params
    # ctx

mkVerifyAuth ::
  ClosedTerm
    ( PMintParams
        :--> PScriptContext
        :--> POpaque
    )
mkVerifyAuth = plam $ \params ctx -> P.do
  ptrace "enter ========== mkVerifyAuth"
  ctxFields <- pletFields @'["txInfo", "purpose"] ctx
  adminKey <- plet $ pfield @"adminKey" # params
  ptrace ("adminKey: " <> pshow adminKey)
  txInfo <- pletFields @'["referenceInputs", "signatories", "fee"] ctxFields.txInfo
  ptrace "enter ========== after txInfo extracted"

  ptrace ("txInfo.fee: " <> pshow txInfo.fee)
  ptrace ("txInfo.referenceInputs: " <> pshow txInfo.referenceInputs)
  ptrace ("ctxFields.purpose: " <> pshow ctxFields.purpose)
  ptrace ("txInfo.signatories: " <> pshow txInfo.signatories)

  -- 1. Admin signed tx
  passert "The user didn`t sign the transaction" $
    ptxSignedBy # txInfo.signatories # pdata adminKey

  -- 2. Purpose is minting
  passert "Invalid tx purpose" $
    pmatch ctxFields.purpose $ \case
      PMinting _ -> pconstant True
      _ -> pconstant False

  passert "Stop mint script!!!" $ pconstant False

  popaque $ pcon PUnit

-- For tests

newtype MintParams = MintParams
  { adminKey :: PubKeyHash
  }
  deriving stock (Eq, Show)

makeIsDataIndexed ''MintParams [('MintParams, 0)]

deriving via
  (DerivePConstantViaData MintParams PMintParams)
  instance
    (PConstantDecl MintParams)

instance PUnsafeLiftDecl PMintParams where
  type PLifted PMintParams = MintParams
