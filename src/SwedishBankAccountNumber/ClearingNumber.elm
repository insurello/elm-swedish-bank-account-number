module SwedishBankAccountNumber.ClearingNumber exposing (ClearingNumber, Error(..), Category(..), fromString, toString, getBankName)

{-| Validating a bank account number is a two-step process: First you need to
validate the clearing number. Why?

  - To know which bank the bank account number is for, and as such which
    validation rules to use.
  - Newer “type 1” accounts include the clearing number in the checksum calculation.

This module lets you validate clearing numbers.

@docs ClearingNumber, Error, Category, fromString, toString, getBankName

-}

import SwedishBankAccountNumber.Bank as Bank
import SwedishBankAccountNumber.ClearingNumberInternal exposing (ClearingNumberInternal(..))


{-| Represents a Swedish clearing number.

The only way to create a value of the `ClearingNumber` type is by calling
`fromString`. This way, if you encounter a `ClearingNumber`, you know that it
is always valid.

`ClearingNumber` also contains information about which Swedish bank
the bank account number is for.

-}
type alias ClearingNumber =
    ClearingNumberInternal


{-| Trying to construct a `ClearingNumber` can fail in two ways:

  - The clearing number is too short or too long. `BadLength`
    contains how many digits were actually passed. Clearing numbers are
    always 4 or 5 digits long.
  - The clearing number is unknown.

-}
type Error
    = BadLength Int
    | Unknown


{-| Banks are categorized into three categories:

  - `Standard`: The bank is active, and part of both the Bankgiro and Data clearing systems.
  - `DataclearingOnly`: The bank is active, part of the Data clearing system,
    but _not_ part of the Bankgiro system. They are marked with “(deltar endast i
    Dataclearingen, ej i Bankgirosystemet)” in Bankgirot’s PDF. (Bankgirot is a
    Swedish Clearing organization, while “Dataclearingen” is a transfer system owned
    by the Swedish Bankers’ Association.)
  - `Historical`: The bank does not exist anymore.

-}
type Category
    = Standard
    | DataclearingOnly
    | Historical


convertCategory : Bank.Category -> Category
convertCategory bankCategory =
    case bankCategory of
        Bank.Standard ->
            Standard

        Bank.DataclearingOnly ->
            DataclearingOnly

        Bank.Historical ->
            Historical


{-| Validate and construct a `ClearingNumber`.

The clearing number string is allowed to have any kind of crazy formatting.
The function simply takes all the digits of the string and discards all other
characters. The following all mean the same thing:

    9420
    94 20
    94,20
    abc94-2#0!

If you have a form field for the clearing number, you might want to normalize
it on blur. You can use `String.filter Char.isDigit myString` to do so.

The function also returns the [Category](#Category) of the bank, to remind you
to case on the different categories:

  - `Standard`: All is good to go!
  - `DataclearingOnly`: If your system only can make Bankgiro transfers, you
    might want to show an error message for this category.
  - `Historical`: You probably want to show a nice error message saying that the
    bank does not exist anymore.

-}
fromString : String -> Result Error ( Category, ClearingNumber )
fromString string =
    let
        digits =
            String.filter Char.isDigit string

        numDigits =
            String.length digits

        maybeInt =
            if numDigits == 4 || numDigits == 5 then
                String.toInt digits

            else
                Nothing
    in
    case maybeInt of
        Just clearing ->
            case Bank.fromClearing clearing of
                Just bank ->
                    Ok
                        ( convertCategory (Bank.getCategory bank)
                        , ClearingNumber bank (String.fromInt clearing)
                        )

                Nothing ->
                    Err Unknown

        Nothing ->
            Err (BadLength numDigits)


{-| Get a string containing digits only (no hyphens or spaces or anything),
such as “9420”.
-}
toString : ClearingNumber -> String
toString (ClearingNumber _ clearingNumber) =
    clearingNumber


{-| Get a human readable string name of the bank identified from the clearing
number, such as “Länsförsäkringar Bank”.
-}
getBankName : ClearingNumber -> String
getBankName (ClearingNumber bank _) =
    Bank.getName bank
