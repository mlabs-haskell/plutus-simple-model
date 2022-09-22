module Plutus.Model.Mock.ProtocolParameters (
  PParams (..),
  readAlonzoParams,
  -- readBabbageParams,
  defaultAlonzoParams,
  defaultBabbageParams,
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
import Cardano.Ledger.Babbage.Translation qualified as B
import Cardano.Ledger.BaseTypes qualified as Alonzo
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (StandardCrypto)

import PlutusCore (defaultCostModelParams)

-- | Type that unifies protocol parameters across eras.
data PParams
  = -- | alonzo era protocol parameters
    AlonzoParams (Alonzo.PParams (AlonzoEra StandardCrypto))
  | -- | babbage era protocol parameters
    BabbageParams (Babbage.PParams (BabbageEra StandardCrypto))

-- | Reads protocol parameters from file.
readAlonzoParams :: FilePath -> IO PParams
readAlonzoParams = fmap AlonzoParams . readJson

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

defaultAlonzoParams' :: Alonzo.PParams (AlonzoEra StandardCrypto)
defaultAlonzoParams' =
  Alonzo.PParams
    { Alonzo._minfeeA = 44
    , Alonzo._minfeeB = 155381
    , Alonzo._maxBBSize = 65536
    , Alonzo._maxTxSize = 20000
    , Alonzo._maxBHSize = 1100
    , Alonzo._keyDeposit = Coin 0
    , Alonzo._poolDeposit = Coin 0
    , Alonzo._eMax = 18
    , Alonzo._nOpt = 100
    , Alonzo._a0 = rational 0
    , Alonzo._rho = rational 0
    , Alonzo._tau = rational 0
    , Alonzo._d = rational 0.7
    , Alonzo._extraEntropy = Alonzo.NeutralNonce
    , Alonzo._protocolVersion = Alonzo.ProtVer {Alonzo.pvMajor = 6, Alonzo.pvMinor = 0}
    , Alonzo._minPoolCost = Coin 0
    , Alonzo._coinsPerUTxOWord = Coin 34482
    , Alonzo._costmdls = defaultCostModels
    , Alonzo._prices =
        Alonzo.Prices
          { Alonzo.prMem = rational 0.0577
          , Alonzo.prSteps = rational 7.21e-05
          }
    , Alonzo._maxTxExUnits = Alonzo.ExUnits 30000000 10000000000
    , Alonzo._maxBlockExUnits = Alonzo.ExUnits 50000000 40000000000
    , Alonzo._maxValSize = 5000
    , Alonzo._collateralPercentage = 150
    , Alonzo._maxCollateralInputs = 3
    }

defaultCostModels :: Alonzo.CostModels
defaultCostModels =
  Alonzo.CostModels $
    Map.fromList $
      fmap toCostModel [PlutusV1, PlutusV2]
  where
    toCostModel lang = (lang, fromRight (error "Cost model apply fail") $ Alonzo.mkCostModel lang cost)
    cost = fromJust defaultCostModelParams

-- Babbage

-- | Default Babbage era parameters
defaultBabbageParams :: PParams
defaultBabbageParams = BabbageParams $ B.translatePParams defaultAlonzoParams'
