-- | Oracle example with script that uses hashed datum for Oracle reference input.
module Suites.Plutus.Model.Script.V2.Onchain.Oracle.Hashed (
  Bet,
  betScript,
) where

import Plutus.Model.V2 (TypedValidator, datumOf, mkTypedValidator, toBuiltinValidator)
import PlutusTx qualified

import Suites.Plutus.Model.Script.V2.Onchain.Oracle

import Prelude (($))
import PlutusCore.Version (plcVersion100)

type Bet = TypedValidator BetDatum BetAct

{- HLINT ignore betScript "Avoid lambda" -}

-- | The GeroGov validator script instance
betScript :: BetParams -> Bet
betScript betParams =
  mkTypedValidator $
    $$(PlutusTx.compile [||\param -> toBuiltinValidator (betContract datumOf param)||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 betParams
