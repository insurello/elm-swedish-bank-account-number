module SwedishBankAccountNumber.ClearingNumberInternal exposing (ClearingNumberInternal(..))

import SwedishBankAccountNumber.Bank exposing (Bank)


type ClearingNumberInternal
    = ClearingNumber Bank String
