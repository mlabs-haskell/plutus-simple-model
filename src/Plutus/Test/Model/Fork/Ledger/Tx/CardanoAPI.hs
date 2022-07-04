{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}

{-|
Interface to the transaction types from 'cardano-api'
-}
module Plutus.Test.Model.Fork.Ledger.Tx.CardanoAPI(
  CardanoBuildTx(..)
  , SomeCardanoApiTx(..)
  , withIsCardanoEra
  , fromTxScriptValidity
  , toTxScriptValidity
  , plutusScriptsFromTxBody
  , fromCardanoTxId
  , toCardanoTxIn
  , toCardanoTxInsCollateral
  , toCardanoTxInWitness
  , toCardanoTxOut
  , toCardanoTxOutUnsafe
  , toCardanoTxOutDatumHash
  , toCardanoTxOutValue
  , toCardanoAddressInEra
  , toCardanoMintValue
  , toCardanoValue
  , toCardanoFee
  , toCardanoValidityRange
  , toCardanoScriptInEra
  , toCardanoPaymentKeyHash
  , toCardanoScriptData
  , toCardanoScriptDataHash
  , toCardanoScriptHash
  , toCardanoTxId
  , ToCardanoError(..)
  , FromCardanoError(..)
  , deserialiseFromRawBytes
) where

import Prelude

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Core qualified as Ledger
import Codec.Serialise (Serialise, deserialiseOrFail)
import Codec.Serialise qualified as Codec
import Codec.Serialise.Decoding (Decoder, decodeBytes, decodeSimple)
import Codec.Serialise.Encoding (Encoding (Encoding), Tokens (TkBytes, TkSimple))
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Plutus.Test.Model.Fork.Ledger.Address qualified as P
import Plutus.Test.Model.Fork.Ledger.Scripts qualified as P
import Plutus.Test.Model.Fork.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Api qualified as Api
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Credential qualified as Credential
import Plutus.V1.Ledger.Scripts qualified as P
import Plutus.Test.Model.Fork.Ledger.Slot qualified as P
import Plutus.V1.Ledger.Tx qualified as P
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty), colon, viaShow, (<+>))

newtype CardanoBuildTx = CardanoBuildTx { getCardanoBuildTx :: C.TxBodyContent C.BuildTx C.AlonzoEra }
  deriving (Eq, Show)

instance ToJSON CardanoBuildTx where
  toJSON = error "TODO: ToJSON CardanoBuildTx"

instance FromJSON CardanoBuildTx where
  parseJSON _ = parseFail "TODO: FromJSON CardanoBuildTx"

instance Pretty CardanoBuildTx where
  pretty (CardanoBuildTx txBodyContent) = viaShow txBodyContent

-- | Cardano tx from any era.
data SomeCardanoApiTx where
  SomeTx :: C.IsCardanoEra era => C.Tx era -> C.EraInMode era C.CardanoMode -> SomeCardanoApiTx

instance Eq SomeCardanoApiTx where
  (SomeTx tx1 C.ByronEraInCardanoMode) == (SomeTx tx2 C.ByronEraInCardanoMode)     = tx1 == tx2
  (SomeTx tx1 C.ShelleyEraInCardanoMode) == (SomeTx tx2 C.ShelleyEraInCardanoMode) = tx1 == tx2
  (SomeTx tx1 C.AllegraEraInCardanoMode) == (SomeTx tx2 C.AllegraEraInCardanoMode) = tx1 == tx2
  (SomeTx tx1 C.MaryEraInCardanoMode) == (SomeTx tx2 C.MaryEraInCardanoMode)       = tx1 == tx2
  (SomeTx tx1 C.AlonzoEraInCardanoMode) == (SomeTx tx2 C.AlonzoEraInCardanoMode)   = tx1 == tx2
  _ == _                                                                           = False

deriving instance Show SomeCardanoApiTx

instance Pretty SomeCardanoApiTx where
  pretty = viaShow

instance Serialise SomeCardanoApiTx where
  encode (SomeTx tx eraInMode) = encodedMode eraInMode <> Encoding (TkBytes (C.serialiseToCBOR tx))
    where
      encodedMode :: C.EraInMode era C.CardanoMode -> Encoding
      -- 0 and 1 are for ByronEraInByronMode and ShelleyEraInShelleyMode
      encodedMode C.ByronEraInCardanoMode   = Encoding (TkSimple 2)
      encodedMode C.ShelleyEraInCardanoMode = Encoding (TkSimple 3)
      encodedMode C.AllegraEraInCardanoMode = Encoding (TkSimple 4)
      encodedMode C.MaryEraInCardanoMode    = Encoding (TkSimple 5)
      encodedMode C.AlonzoEraInCardanoMode  = Encoding (TkSimple 6)
  decode = do
    w <- decodeSimple
    case w of
      2 -> decodeTx C.AsByronEra C.ByronEraInCardanoMode
      3 -> decodeTx C.AsShelleyEra C.ShelleyEraInCardanoMode
      4 -> decodeTx C.AsAllegraEra C.AllegraEraInCardanoMode
      5 -> decodeTx C.AsMaryEra C.MaryEraInCardanoMode
      6 -> decodeTx C.AsAlonzoEra C.AlonzoEraInCardanoMode
      _ -> fail "Unexpected value while decoding Cardano.Api.EraInMode"
    where
      decodeTx :: C.IsCardanoEra era => C.AsType era -> C.EraInMode era C.CardanoMode -> Decoder s SomeCardanoApiTx
      decodeTx asType eraInMode = do
        bytes <- decodeBytes
        tx <- either (const $ fail "Failed to decode Cardano.Api.Tx") pure $ C.deserialiseFromCBOR (C.AsTx asType) bytes
        pure $ SomeTx tx eraInMode

instance ToJSON SomeCardanoApiTx where
  toJSON (SomeTx tx eraInMode) =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx
           , "eraInMode" .= eraInMode
           ]

-- | Converting 'SomeCardanoApiTx' to JSON.
--
-- If the "tx" field is from an unknown era, the JSON parser will print an
-- error at runtime while parsing.
instance FromJSON SomeCardanoApiTx where
  parseJSON v = parseByronInCardanoModeTx v
            <|> parseShelleyEraInCardanoModeTx v
            <|> parseAllegraEraInCardanoModeTx v
            <|> parseMaryEraInCardanoModeTx v
            <|> parseAlonzoEraInCardanoModeTx v
            <|> parseEraInCardanoModeFail v

-- | Run code that needs an `IsCardanoEra` constraint while you only have an `EraInMode` value.
withIsCardanoEra :: C.EraInMode era C.CardanoMode -> (C.IsCardanoEra era => r) -> r
withIsCardanoEra C.ByronEraInCardanoMode r   = r
withIsCardanoEra C.ShelleyEraInCardanoMode r = r
withIsCardanoEra C.AllegraEraInCardanoMode r = r
withIsCardanoEra C.MaryEraInCardanoMode r    = r
withIsCardanoEra C.AlonzoEraInCardanoMode r  = r

parseByronInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseByronInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse ByronEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsByronEra)

parseShelleyEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseShelleyEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse ShelleyEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsShelleyEra)

parseMaryEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseMaryEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse MaryEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsMaryEra)

parseAllegraEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseAllegraEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse AllegraEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsAllegraEra)

parseAlonzoEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseAlonzoEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse AlonzoEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsAlonzoEra)

parseEraInCardanoModeFail :: Aeson.Value -> Parser SomeCardanoApiTx
parseEraInCardanoModeFail _ = fail "Unable to parse 'eraInMode'"

parseSomeCardanoTx
  :: ( FromJSON (C.EraInMode era C.CardanoMode)
     , C.IsCardanoEra era
     )
  => String
  -> C.AsType (C.Tx era)
  -> Aeson.Value
  -> Parser SomeCardanoApiTx
parseSomeCardanoTx errorMsg txAsType (Aeson.Object v) =
  SomeTx
    <$> (v .: "tx" >>= \envelope -> either (const $ parseFail errorMsg)
                                           pure
                                           $ C.deserialiseFromTextEnvelope txAsType envelope)
    <*> v .: "eraInMode"
parseSomeCardanoTx _ _ invalid =
    prependFailure "parsing SomeCardanoApiTx failed, "
      (typeMismatch "Object" invalid)

-- | Given a 'C.TxScriptValidity era', if the @era@ supports scripts, return a
-- @True@ or @False@ depending on script validity. If the @era@ does not support
-- scripts, always return @True@.
fromTxScriptValidity :: C.TxScriptValidity era -> Bool
fromTxScriptValidity C.TxScriptValidityNone                                                      = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid)   = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid) = False

toTxScriptValidity :: Bool -> C.TxScriptValidity C.AlonzoEra
toTxScriptValidity True  = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid
toTxScriptValidity False = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid

-- | Extract plutus scripts from a Cardano API tx body.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
plutusScriptsFromTxBody :: C.TxBody era -> Map P.ScriptHash P.Script
plutusScriptsFromTxBody C.ByronTxBody {} = mempty
plutusScriptsFromTxBody (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $ mapMaybe (fromLedgerScript shelleyBasedEra) scripts

-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript
  :: C.ShelleyBasedEra era
  -> Ledger.Script (C.ShelleyLedgerEra era)
  -> Maybe (P.ScriptHash, P.Script)
fromLedgerScript C.ShelleyBasedEraShelley _     = Nothing
fromLedgerScript C.ShelleyBasedEraAllegra _     = Nothing
fromLedgerScript C.ShelleyBasedEraMary _        = Nothing
fromLedgerScript C.ShelleyBasedEraAlonzo script = fromAlonzoLedgerScript script

-- | Convert a script the Alonzo era to a Plutus script along with it's hash.
fromAlonzoLedgerScript :: Alonzo.Script a -> Maybe (P.ScriptHash, P.Script)
fromAlonzoLedgerScript Alonzo.TimelockScript {} = Nothing
fromAlonzoLedgerScript (Alonzo.PlutusScript _ bs) =
  let script = fmap (\s -> (P.scriptHash s, s))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script


toCardanoTxIn :: P.TxOutRef -> Either ToCardanoError C.TxIn
toCardanoTxIn (P.TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromCardanoTxId :: C.TxId -> P.TxId
fromCardanoTxId txId = P.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

toCardanoTxId :: P.TxId -> Either ToCardanoError C.TxId
toCardanoTxId (P.TxId bs) =
    tag "toCardanoTxId"
    $ deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs


toCardanoTxInsCollateral :: Set.Set P.TxIn -> Either ToCardanoError (C.TxInsCollateral C.AlonzoEra)
toCardanoTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInAlonzoEra) . traverse (toCardanoTxIn . P.txInRef) . Set.toList

toCardanoTxInWitness :: P.TxInType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.AlonzoEra)
toCardanoTxInWitness P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness P.ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness
    (P.ConsumeScriptAddress
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
        <$> (C.PScript <$> toCardanoPlutusScript validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )

toCardanoMintWitness :: P.Redeemers -> Int -> P.MintingPolicy -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.AlonzoEra)
toCardanoMintWitness redeemers idx (P.MintingPolicy script) = do
    let redeemerPtr = P.RedeemerPtr P.Mint (fromIntegral idx)
    P.Redeemer redeemer <- maybe (Left MissingMintingPolicyRedeemer) Right (Map.lookup redeemerPtr redeemers)
    C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
        <$> (C.PScript <$> toCardanoPlutusScript script)
        <*> pure C.NoScriptDatumForMint
        <*> pure (C.fromPlutusData $ Api.toData redeemer)
        <*> pure zeroExecutionUnits
{-
fromCardanoTxOut :: C.TxOut C.CtxTx era -> Either FromCardanoError P.TxOut
fromCardanoTxOut (C.TxOut addr value datum) =
    P.TxOut
    <$> fromCardanoAddressInEra addr
    <*> pure (fromCardanoTxOutValue value)
    <*> pure (fromCardanoTxOutDatum datum)
-}

toCardanoTxOut
    :: C.NetworkId
    -> (Maybe P.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.AlonzoEra))
    -> P.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.AlonzoEra)
toCardanoTxOut networkId fromHash (P.TxOut addr value datumHash) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValue value
            <*> fromHash datumHash
            <*> pure C.ReferenceScriptNone

toCardanoTxOutUnsafe
    :: C.NetworkId
    -> (Maybe P.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.AlonzoEra))
    -> P.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.AlonzoEra)
toCardanoTxOutUnsafe networkId fromHash (P.TxOut addr value datumHash) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValueUnsafe value
            <*> fromHash datumHash
            <*> pure C.ReferenceScriptNone

{-
lookupDatum :: Map P.DatumHash P.Datum -> Maybe P.DatumHash -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.AlonzoEra)
lookupDatum datums datumHash =
    case flip Map.lookup datums =<< datumHash of
        Just datum -> pure $ C.TxOutDatum C.ScriptDataInAlonzoEra (toCardanoScriptData $ P.getDatum datum)
        Nothing    -> toCardanoTxOutDatumHash datumHash
-}

toCardanoAddressInEra :: C.NetworkId -> P.Address -> Either ToCardanoError (C.AddressInEra C.AlonzoEra)
toCardanoAddressInEra networkId (P.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) <$>
        (C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

toCardanoPaymentCredential :: Credential.Credential -> Either ToCardanoError C.PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash (P.PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoPaymentKeyHash :: P.PaymentPubKeyHash -> Either ToCardanoError (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (P.PaymentPubKeyHash (P.PubKeyHash bs)) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

toCardanoScriptHash :: P.ValidatorHash -> Either ToCardanoError C.ScriptHash
toCardanoScriptHash (P.ValidatorHash bs) = tag "toCardanoScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin bs

toCardanoStakeAddressReference :: Maybe Credential.StakingCredential -> Either ToCardanoError C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (Credential.StakingHash credential)) =
    C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just Credential.StakingPtr{}) = Left StakingPointersNotSupported

toCardanoStakeCredential :: Credential.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential (Credential.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (Credential.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoStakeKeyHash :: P.PubKeyHash -> Either ToCardanoError (C.Hash C.StakeKey)
toCardanoStakeKeyHash (P.PubKeyHash bs) = tag "toCardanoStakeKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PlutusTx.fromBuiltin bs)

toCardanoTxOutValue :: P.Value -> Either ToCardanoError (C.TxOutValue C.AlonzoEra)
toCardanoTxOutValue value = do
    when (Ada.fromValue value == mempty) (Left OutputHasZeroAda)
    C.TxOutValue C.MultiAssetInAlonzoEra <$> toCardanoValue value

toCardanoTxOutValueUnsafe :: P.Value -> Either ToCardanoError (C.TxOutValue C.AlonzoEra)
toCardanoTxOutValueUnsafe value = C.TxOutValue C.MultiAssetInAlonzoEra <$> toCardanoValue value

toCardanoTxOutDatumHash :: Maybe P.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.AlonzoEra)
toCardanoTxOutDatumHash Nothing          = pure C.TxOutDatumNone
toCardanoTxOutDatumHash (Just datumHash) = C.TxOutDatumHash C.ScriptDataInAlonzoEra <$> toCardanoScriptDataHash datumHash

toCardanoScriptDataHash :: P.DatumHash -> Either ToCardanoError (C.Hash C.ScriptData)
toCardanoScriptDataHash (P.DatumHash bs) = tag "toCardanoTxOutDatumHash" (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs))

toCardanoMintValue :: P.Redeemers -> P.Value -> Set.Set P.MintingPolicy -> Either ToCardanoError (C.TxMintValue C.BuildTx C.AlonzoEra)
toCardanoMintValue redeemers value mps =
    C.TxMintValue C.MultiAssetInAlonzoEra
        <$> toCardanoValue value
        <*> (C.BuildTxWith . Map.fromList <$> traverse (\(idx, mp) -> (,) <$> (toCardanoPolicyId . P.mintingPolicyHash) mp <*> toCardanoMintWitness redeemers idx mp) (Prelude.zip [0..] $ Set.toList mps))

toCardanoValue :: P.Value -> Either ToCardanoError C.Value
toCardanoValue = fmap C.valueFromList . traverse fromValue . Value.flattenValue
    where
        fromValue (currencySymbol, tokenName, amount)
            | currencySymbol == Ada.adaSymbol && tokenName == Ada.adaToken =
                pure (C.AdaAssetId, C.Quantity amount)
            | otherwise =
                (,) <$> (C.AssetId <$> toCardanoPolicyId (Value.currencyMPSHash currencySymbol) <*> pure (toCardanoAssetName tokenName)) <*> pure (C.Quantity amount)

toCardanoPolicyId :: P.MintingPolicyHash -> Either ToCardanoError C.PolicyId
toCardanoPolicyId (P.MintingPolicyHash bs) = C.PolicyId <$> tag "toCardanoPolicyId" (tag (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes") (deserialiseFromRawBytes C.AsScriptHash (PlutusTx.fromBuiltin bs)))

toCardanoAssetName :: Value.TokenName -> C.AssetName
toCardanoAssetName (Value.TokenName bs) = C.AssetName $ PlutusTx.fromBuiltin bs

toCardanoFee :: P.Value -> Either ToCardanoError (C.TxFee C.AlonzoEra)
toCardanoFee value = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra <$> toCardanoLovelace value

toCardanoLovelace :: P.Value -> Either ToCardanoError C.Lovelace
toCardanoLovelace value = if value == Ada.lovelaceValueOf lovelace then pure . C.quantityToLovelace . C.Quantity $ lovelace else Left ValueNotPureAda
    where
        Ada.Lovelace lovelace = Ada.fromValue value

toCardanoValidityRange :: P.SlotRange -> Either ToCardanoError (C.TxValidityLowerBound C.AlonzoEra, C.TxValidityUpperBound C.AlonzoEra)
toCardanoValidityRange (P.Interval l u) = (,) <$> toCardanoValidityLowerBound l <*> toCardanoValidityUpperBound u

toCardanoValidityLowerBound :: P.LowerBound P.Slot -> Either ToCardanoError (C.TxValidityLowerBound C.AlonzoEra)
toCardanoValidityLowerBound (P.LowerBound P.NegInf _) = pure C.TxValidityNoLowerBound
toCardanoValidityLowerBound (P.LowerBound (P.Finite slotNo) closed)
    = pure . C.TxValidityLowerBound C.ValidityLowerBoundInAlonzoEra . toCardanoSlotNo $ if slotNo < 0 then 0 else if closed then slotNo else slotNo + 1
toCardanoValidityLowerBound (P.LowerBound P.PosInf _) = Left InvalidValidityRange

toCardanoValidityUpperBound :: P.UpperBound P.Slot -> Either ToCardanoError (C.TxValidityUpperBound C.AlonzoEra)
toCardanoValidityUpperBound (P.UpperBound P.PosInf _) = pure $ C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra
toCardanoValidityUpperBound (P.UpperBound (P.Finite slotNo) closed)
    = pure . C.TxValidityUpperBound C.ValidityUpperBoundInAlonzoEra . toCardanoSlotNo $ if closed then slotNo + 1 else slotNo
toCardanoValidityUpperBound (P.UpperBound P.NegInf _) = Left InvalidValidityRange

toCardanoSlotNo :: P.Slot -> C.SlotNo
toCardanoSlotNo (P.Slot i) = C.SlotNo (fromInteger i)

toCardanoScriptData :: Api.BuiltinData -> C.ScriptData
toCardanoScriptData = C.fromPlutusData . Api.builtinDataToData

toCardanoScriptInEra :: P.Script -> Either ToCardanoError (C.ScriptInEra C.AlonzoEra)
toCardanoScriptInEra script = C.ScriptInEra C.PlutusScriptV1InAlonzo . C.PlutusScript C.PlutusScriptV1 <$> toCardanoPlutusScript script

toCardanoPlutusScript :: P.Script -> Either ToCardanoError (C.PlutusScript C.PlutusScriptV1)
toCardanoPlutusScript =
    tag "toCardanoPlutusScript"
    . deserialiseFromRawBytes (C.AsPlutusScript C.AsPlutusScriptV1) . BSL.toStrict . Codec.serialise

deserialiseFromRawBytes :: C.SerialiseAsRawBytes t => C.AsType t -> ByteString -> Either ToCardanoError t
deserialiseFromRawBytes asType = maybe (Left DeserialisationError) Right . C.deserialiseFromRawBytes asType

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (Tag s)

data FromCardanoError
    = SimpleScriptsNotSupported
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty FromCardanoError where
    pretty SimpleScriptsNotSupported        = "Simple scripts are not supported"

data ToCardanoError
    = TxBodyError String -- ^ A C.TxBodyError converted to String
    | DeserialisationError
    | InvalidValidityRange
    | ValueNotPureAda
    | OutputHasZeroAda
    | StakingPointersNotSupported
    | SimpleScriptsNotSupportedToCardano
    | MissingTxInType
    | MissingMintingPolicyRedeemer
    | MissingMintingPolicy
    | ScriptPurposeNotSupported P.ScriptTag
    | Tag String ToCardanoError
    deriving stock (Show, Eq, Generic)

instance Pretty ToCardanoError where
    pretty (TxBodyError err)                  = "TxBodyError" <> colon <+> pretty err
    pretty DeserialisationError               = "ByteString deserialisation failed"
    pretty InvalidValidityRange               = "Invalid validity range"
    pretty ValueNotPureAda                    = "Fee values should only contain Ada"
    pretty OutputHasZeroAda                   = "Transaction outputs should not contain zero Ada"
    pretty StakingPointersNotSupported        = "Staking pointers are not supported"
    pretty SimpleScriptsNotSupportedToCardano = "Simple scripts are not supported"
    pretty MissingTxInType                    = "Missing TxInType"
    pretty MissingMintingPolicyRedeemer       = "Missing minting policy redeemer"
    pretty MissingMintingPolicy               = "Missing minting policy"
    pretty (ScriptPurposeNotSupported p)      = "Script purpose not supported:" <+> viaShow p
    pretty (Tag t err)                        = pretty t <> colon <+> pretty err

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0
