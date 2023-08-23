module Plutus.Model.Mock.ProtocolParameters (
  PParams (..),
  readAlonzoParams,
  readBabbageParams,
  defaultAlonzoParams,
  defaultBabbageParamsV1,
  defaultBabbageParamsV2,
  customAlonzoParams,
  customBabbageParams,
) where

import Prelude

import Data.Aeson
import Data.Either
import Data.Map.Strict qualified as Map
import Data.Maybe

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams qualified as Babbage
import Cardano.Ledger.BaseTypes qualified as Alonzo
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Core as C

import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
import Data.Functor.Identity (Identity)
import Cardano.Ledger.Alonzo.Core (CoinPerWord(CoinPerWord))
import Data.Coerce (coerce)

-- | Type that unifies protocol parameters across eras.
data PParams
  = -- | alonzo era protocol parameters
    AlonzoParams (C.PParams (AlonzoEra StandardCrypto))
  | -- | babbage era protocol parameters
    BabbageParams (C.PParams (BabbageEra StandardCrypto))

-- | Mutate the default params
customAlonzoParams ::
  ( C.PParams (AlonzoEra StandardCrypto) ->
    C.PParams (AlonzoEra StandardCrypto)
  ) ->
  PParams
customAlonzoParams f = AlonzoParams $ f defaultAlonzoParams'

-- | Mutate the default params
customBabbageParams ::
  ( C.PParams (BabbageEra StandardCrypto) ->
    C.PParams (BabbageEra StandardCrypto)
  ) ->
  PParams
customBabbageParams f = BabbageParams (f defaultBabbageParams')

-- | Reads protocol parameters from file.
readAlonzoParams :: FilePath -> IO PParams
readAlonzoParams = fmap AlonzoParams . readJson

-- | Reads protocol parameters from file.
readBabbageParams :: FilePath -> IO PParams
readBabbageParams = fmap (BabbageParams . C.upgradePParams ()) . readJson

readJson :: (FromJSON a) => FilePath -> IO a
readJson = fmap fromJust . decodeFileStrict'

rational :: Alonzo.BoundedRational r => Rational -> r
rational = fromJust . Alonzo.boundRational

-----------------------------------------------------------------------
-- default params

-- Alonzo

-- | Default Alonzo era parameters
defaultAlonzoParams :: PParams
defaultAlonzoParams = AlonzoParams defaultAlonzoParams'

defaultAlonzoParams' :: C.PParams (AlonzoEra StandardCrypto)
defaultAlonzoParams' =
  let
    app :: Alonzo.AlonzoPParams Identity (AlonzoEra StandardCrypto) =
      Alonzo.AlonzoPParams
        { Alonzo.appMinFeeA = Coin 44
        , Alonzo.appMinFeeB = Coin 155381
        , Alonzo.appMaxBBSize = 65536
        , Alonzo.appMaxTxSize = 20000
        , Alonzo.appMaxBHSize = 1100
        , Alonzo.appKeyDeposit = Coin 0
        , Alonzo.appPoolDeposit = Coin 0
        , Alonzo.appEMax = 18
        , Alonzo.appNOpt = 100
        , Alonzo.appA0 = rational 0
        , Alonzo.appRho = rational 0
        , Alonzo.appTau = rational 0
        , Alonzo.appD = rational 0.7
        , Alonzo.appExtraEntropy = Alonzo.NeutralNonce
        , Alonzo.appProtocolVersion = Alonzo.ProtVer {Alonzo.pvMajor = C.eraProtVerHigh @(AlonzoEra StandardCrypto), Alonzo.pvMinor = 0}
        , Alonzo.appMinPoolCost = Coin 0
        , Alonzo.appCoinsPerUTxOWord = CoinPerWord (Coin 34482)
        , Alonzo.appCostModels = defaultCostModels
        , Alonzo.appPrices =
            Alonzo.Prices
              { Alonzo.prMem = rational 0.0577
              , Alonzo.prSteps = rational 7.21e-05
              }
        , Alonzo.appMaxTxExUnits = Alonzo.OrdExUnits $ Alonzo.ExUnits 30000000 10000000000
        , Alonzo.appMaxBlockExUnits = Alonzo.OrdExUnits $ Alonzo.ExUnits 50000000 40000000000
        , Alonzo.appMaxValSize = 5000
        , Alonzo.appCollateralPercentage = 150
        , Alonzo.appMaxCollateralInputs = 3
        }
  in coerce app

defaultBabbageParams' :: C.PParams (BabbageEra StandardCrypto)
defaultBabbageParams' = C.upgradePParams () defaultAlonzoParams'

defaultCostModels :: Alonzo.CostModels
defaultCostModels =
  (\validCM -> Alonzo.CostModels validCM mempty mempty) $
    Map.fromList $
      fmap toCostModel [PlutusV1, PlutusV2]
  where
    toCostModel lang = (lang, fromRight (error "Cost model apply fail") $ Alonzo.mkCostModel lang $ Map.elems cost)
    cost = fromJust defaultCostModelParams

-- Babbage

-- | Default Babbage V1 era parameters
defaultBabbageParamsV1 :: PParams
defaultBabbageParamsV1 = BabbageParams defaultBabbageParams'

-- | Default Babbage V2 era parameters
defaultBabbageParamsV2 :: PParams
defaultBabbageParamsV2 =
  let old = coerce defaultBabbageParams'
  in
    BabbageParams
      $ coerce $ old
        { Babbage.bppProtocolVersion =
            Alonzo.ProtVer {pvMajor = C.eraProtVerHigh @(BabbageEra StandardCrypto), pvMinor = 0}
        }
