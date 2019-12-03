module SwedishBankAccountNumber exposing
    ( SwedishBankAccountNumber, Error(..), fromStringUsingClearingNumber, toRecord
    , AccountNumberLength(..), getAccountNumberLength
    )

{-| This library lets you validate Swedish bank account numbers.

First, validate a clearing number using [SwedishBankAccountNumber.ClearingNumber.fromString](./SwedishBankAccountNumber-ClearingNumber#fromString).

Then, validate an account number using [SwedishBankAccountNumber.fromStringUsingClearingNumber](#fromStringUsingClearingNumber).


# Main

@docs SwedishBankAccountNumber, Error, fromStringUsingClearingNumber, toRecord


# Helpers

@docs AccountNumberLength, getAccountNumberLength

-}

import SwedishBankAccountNumber.Bank as Bank
import SwedishBankAccountNumber.ClearingNumber exposing (ClearingNumber)
import SwedishBankAccountNumber.ClearingNumberInternal exposing (ClearingNumberInternal(..))


{-| Represents a Swedish bank account number.

This is an example of a valid Swedish bank account number:

    9420 - 4172385

A bank account number consists of two parts:

1.  The clearing number: 9420
2.  The account number: 4172385

The only way to create a value of the `SwedishBankAccountNumber` type is by
calling `fromStringUsingClearingNumber`. This way, if you encounter a
`SwedishBankAccountNumber` you always know that it’s valid.

`SwedishBankAccountNumber` also contains information about which Swedish bank
the bank account number is for.

-}
type SwedishBankAccountNumber
    = SwedishBankAccountNumber ClearingNumber String


{-| Trying to construct a `SwedishBankAccountNumber` can fail in two ways:

  - The account number is too short or too long. `BadAccountNumberLength`
    contains how many digits were actually passed;
    [getAccountNumberLength](#getAccountNumberLength) lets you know how many are valid.
  - The checksum of the bank account number is invalid (the last digit of
    account numbers is a control digit that is supposed to make the checksum
    add up).

(Constructing a `ClearingNumber` can fail in its own ways – see
[SwedishBankAccountNumber.ClearingNumber.Error](./SwedishBankAccountNumber-ClearingNumber#Error).)

-}
type Error
    = BadAccountNumberLength Int
    | BadChecksum


{-| Validate and construct a `SwedishBankAccountNumber`.

First, you need to get a `ClearingNumber` via
[SwedishBankAccountNumber.ClearingNumber.fromString](./SwedishBankAccountNumber-ClearingNumber#fromString).

Then, pass the validated `ClearingNumber` and an unvalidated account number
string to get a full bank account number.

The account number string is allowed to have any kind of crazy formatting.
The function simply takes all the digits of the string and discards all other
characters. The following all mean the same thing:

    4172385
    417 23 85
    417,23,85
    abc417-23#85!

If you have a form field for the account number, you might want to normalize
it on blur. You can use `String.filter Char.isDigit myString` to do so.

-}
fromStringUsingClearingNumber : ClearingNumber -> String -> Result Error SwedishBankAccountNumber
fromStringUsingClearingNumber ((ClearingNumber bank clearingNumberString) as clearingNumber) string =
    let
        digits =
            String.filter Char.isDigit string

        numDigits =
            String.length digits

        isCorrectLength =
            case Bank.getAccountNumberLength bank of
                Bank.FixedLength length ->
                    numDigits == length

                Bank.Range minimum maximum ->
                    numDigits >= minimum && numDigits <= maximum

        clearingInts =
            Bank.toIntList clearingNumberString

        accountInts =
            Bank.toIntList digits
    in
    if isCorrectLength then
        if Bank.validateAccountNumber bank clearingInts accountInts then
            Ok (SwedishBankAccountNumber clearingNumber digits)

        else
            Err BadChecksum

    else
        Err (BadAccountNumberLength numDigits)


{-| When you need to display a `SwedishBankAccountNumber` or send it via HTTP to
your backend, use this function to get all of its data.

  - `bankName` is a human readable string name of the bank identified from the
    clearing number, such as “Länsförsäkringar Bank”.
  - `clearingNumber` and `accountNumber` are strings containing digits only
    (no hyphens or spaces or anything), such as “9420“ and “4172385”, respectively.

-}
toRecord : SwedishBankAccountNumber -> { bankName : String, clearingNumber : String, accountNumber : String }
toRecord (SwedishBankAccountNumber (ClearingNumber bank clearingNumber) accountNumber) =
    { bankName = Bank.getName bank
    , clearingNumber = clearingNumber
    , accountNumber = accountNumber
    }


{-| Represents the length constraints of an account number.

The most common value is `FixedLength 7`, which all modern “type 1” accounts have.

Older “type 2” accounts usually have `FixedLength 10`.

Then there are some special cases:

  - Handelsbanken uses `FixedLength 9`.
  - Swedbank accounts with clearing numbers starting with `8` have 6-10 digits (`Range 6 10`).
  - Nordea Plusgirot use 7-10 digits (`Range 7 10`).

See <https://github.com/jop-io/kontonummer.js/issues/6> for more information.

-}
type AccountNumberLength
    = FixedLength Int
    | Range Int Int


{-| Get the length constraints of an account number. Since banks have different
rules this depends on the clearing number.

Let’s say you have two form fields – one for the clearing number, and one for the
account number. After the clearing number has been filled, you might want to show
how long the account number is expected to be. Then this function will come in handy.

By the way – clearing numbers are always 4 digits, except Swedbank clearing
numbers starting with `8` which are 5 digits.

-}
getAccountNumberLength : ClearingNumber -> AccountNumberLength
getAccountNumberLength (ClearingNumber bank _) =
    case Bank.getAccountNumberLength bank of
        Bank.FixedLength length ->
            FixedLength length

        Bank.Range min max ->
            Range min max
