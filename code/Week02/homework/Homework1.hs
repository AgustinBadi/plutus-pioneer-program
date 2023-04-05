{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Homework1 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile)
import           PlutusTx.Prelude     (Bool, ($), (&&), BuiltinData, traceIfFalse)
import           Utilities            (wrap, writeValidatorToFile)
import           Prelude              (IO)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are True!
mkValidator :: () -> (Bool, Bool) -> PlutusV2.ScriptContext -> Bool
mkValidator _ (x,y) _ = traceIfFalse "The reedemer values aren't True" $ x && y

wrappedVal :: PlutusTx.Prelude.BuiltinData -> PlutusTx.Prelude.BuiltinData -> PlutusTx.Prelude.BuiltinData -> ()
wrappedVal = wrap mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])

saveHM :: IO ()
saveHM = writeValidatorToFile "./Homework1B.plutus" validator

