{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext, to,
                                       mkMintingPolicyScript, TxInfo, ScriptContext (scriptContextTxInfo), TxInfo (txInfoValidRange))
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), traceIfFalse, (&&))
import           Utilities            (wrapPolicy)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx =   traceIfFalse "Deadline has passed" deadlineReached  &&
                                            traceIfFalse "Owner's signature missing" signedByOwner                                    
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx 

        signedByOwner :: Bool 
        signedByOwner = txSignedBy info $ _pkh

        deadlineReached :: Bool
        deadlineReached = contains (to _deadline) $ txInfoValidRange info


{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
