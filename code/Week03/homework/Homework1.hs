{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), 
                                       Validator, mkValidatorScript, TxInfo (txInfoValidRange))
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval (before, after)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, traceIfFalse, ($), (&&), (||))
import           Utilities            (wrapValidator)
import           PlutusTx.Prelude     (Bool (..))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = traceIfFalse "deadline not reached" $
                                (signedByBeneficiary b1pkh && beforeDeadline) ||
                                (signedByBeneficiary b2pkh && afterDeadline)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    b1pkh :: PubKeyHash
    b1pkh = beneficiary1 dat
    b2pkh :: PubKeyHash
    b2pkh = beneficiary2 dat

    signedByBeneficiary :: PubKeyHash -> Bool
    signedByBeneficiary bpkh = traceIfFalse "beneficiary's signature missing" $ txSignedBy info $ bpkh

    -- The current slot is before the deadline
    beforeDeadline :: Bool
    beforeDeadline = after (deadline dat) (txInfoValidRange info)

    -- The current slot is after the deadline
    afterDeadline :: Bool
    afterDeadline = before (deadline dat) (txInfoValidRange info)


{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
