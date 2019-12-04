# elm-swedish-bank-account-number

This library lets you validate Swedish bank account numbers.

Inspired by https://github.com/jop-io/kontonummer.js. Big thanks to the
creators of that package!

[📎 Data sources](https://github.com/insurello/elm-swedish-bank-account-number/issues/1)

## Example

```elm
import SwedishBankAccountNumber exposing (SwedishBankAccountNumber)
import SwedishBankAccountNumber.ClearingNumber exposing (ClearingNumber)


type Validation
    = InvalidClearingNumber String
    | InvalidAccountNumber ClearingNumber String
    | Valid SwedishBankAccountNumber


validate : String -> String -> Validation
validate clearingNumberString accountNumberString =
    case SwedishBankAccountNumber.ClearingNumber.fromString clearingNumberString of
        Ok clearingNumber ->
            case SwedishBankAccountNumber.create clearingNumber accountNumberString of
                Ok bankAccountNumber ->
                    Valid bankAccountNumber

                Err (SwedishBankAccountNumber.BadAccountNumberLength length) ->
                    InvalidAccountNumber clearingNumber
                        ("The account number must be " ++ getAccountNumberLength clearingNumber ++ " digits. You entered " ++ String.fromInt length ++ ".")

                Err SwedishBankAccountNumber.BadChecksum ->
                    InvalidAccountNumber clearingNumber
                        "Invalid account number. Some digit is wrong."

        Err (SwedishBankAccountNumber.ClearingNumber.BadLength length) ->
            InvalidClearingNumber
                ("The clearing number must be 4-5 digits. You entered " ++ String.fromInt length ++ ".")

        Err SwedishBankAccountNumber.ClearingNumber.Unknown ->
            InvalidClearingNumber
                "Unknown clearing number."


getAccountNumberLength : ClearingNumber -> String
getAccountNumberLength clearingNumber =
    case SwedishBankAccountNumber.getAccountNumberLength clearingNumber of
        SwedishBankAccountNumber.FixedLength fixedLength ->
            String.fromInt fixedLength

        SwedishBankAccountNumber.Range minimum maximum ->
            String.fromInt minimum ++ "-" ++ String.fromInt maximum

```

## License

MIT

---

[![Insurello](https://gitcdn.xyz/repo/insurello/elm-swedish-bank-account-number/master/insurello.svg)](https://jobb.insurello.se/departments/product-tech)
