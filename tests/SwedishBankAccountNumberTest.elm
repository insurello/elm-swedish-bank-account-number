module SwedishBankAccountNumberTest exposing (suite)

import Expect
import SwedishBankAccountNumber exposing (SwedishBankAccountNumber)
import SwedishBankAccountNumber.ClearingNumber
import Test exposing (Test, describe, test)


validate : String -> String -> Result String SwedishBankAccountNumber
validate clearingString accountString =
    case SwedishBankAccountNumber.ClearingNumber.fromString clearingString of
        Ok clearingNumber ->
            case SwedishBankAccountNumber.create clearingNumber accountString of
                Ok bankAccountNumber ->
                    Ok bankAccountNumber

                Err (SwedishBankAccountNumber.BadAccountNumberLength length) ->
                    Err ("BadAccountNumberLength " ++ String.fromInt length)

                Err SwedishBankAccountNumber.BadChecksum ->
                    Err "BadChecksum"

        Err (SwedishBankAccountNumber.ClearingNumber.BadLength length) ->
            Err ("ClearingNumber.BadLength " ++ String.fromInt length)

        Err (SwedishBankAccountNumber.ClearingNumber.Unknown digits) ->
            Err ("ClearingNumber.Unknown " ++ digits)


suite : Test
suite =
    describe "SwedishBankAccountNumber"
        [ test "Forex" <|
            \_ ->
                validate "9420" ", 417 23 85"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Forex Bank"
                            , clearingNumber = "9420"
                            , accountNumber = "4172385"
                            }
                        )
        , test "Handelsbanken" <|
            \_ ->
                validate "6789" "123456789"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Handelsbanken"
                            , clearingNumber = "6789"
                            , accountNumber = "123456789"
                            }
                        )
        , test "Swedbank (with a five digit clearing number)" <|
            \_ ->
                validate "8424-4" ",983 189 224-6"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Swedbank"
                            , clearingNumber = "84244"
                            , accountNumber = "9831892246"
                            }
                        )
        , test "Sparbanken Tanum" <|
            \_ ->
                validate "8351-9" "392 242 224-5"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Swedbank"
                            , clearingNumber = "83519"
                            , accountNumber = "3922422245"
                            }
                        )
        , test "Sparbanken i Hudiksvall" <|
            \_ ->
                validate "8129-9" "043 386 711-6"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Swedbank"
                            , clearingNumber = "81299"
                            , accountNumber = "0433867116"
                            }
                        )
        , test "Nordea personkonto" <|
            \_ ->
                validate "3300" "000620-5124"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Nordea"
                            , clearingNumber = "3300"
                            , accountNumber = "0006205124"
                            }
                        )
        , test "Klarna" <|
            \_ ->
                validate "9789" "1111113"
                    |> Result.map SwedishBankAccountNumber.toRecord
                    |> Expect.equal
                        (Ok
                            { bankName = "Klarna Bank"
                            , clearingNumber = "9789"
                            , accountNumber = "1111113"
                            }
                        )
        ]
