module SwedishBankAccountNumberTest exposing (suite)

import Expect
import SwedishBankAccountNumber exposing (SwedishBankAccountNumber)
import SwedishBankAccountNumber.ClearingNumber
import Test exposing (Test, describe, test)


type Error
    = Clearing SwedishBankAccountNumber.ClearingNumber.Error
    | Account SwedishBankAccountNumber.Error


validate : String -> String -> Result Error SwedishBankAccountNumber
validate clearingString accountString =
    case SwedishBankAccountNumber.ClearingNumber.fromString clearingString of
        Ok ( _, clearingNumber ) ->
            case SwedishBankAccountNumber.create clearingNumber accountString of
                Ok bankAccountNumber ->
                    Ok bankAccountNumber

                Err error ->
                    Err (Account error)

        Err error ->
            Err (Clearing error)


suite : Test
suite =
    describe "SwedishBankAccountNumber"
        [ bankSuite
        , clearingNumberSuite
        , describe "Error"
            [ test "Too short" <|
                \_ ->
                    validate "1234" "123456"
                        |> Expect.equal (Err (Account (SwedishBankAccountNumber.BadAccountNumberLength 6)))
            , test "Too long" <|
                \_ ->
                    validate "1234" "12345678"
                        |> Expect.equal (Err (Account (SwedishBankAccountNumber.BadAccountNumberLength 8)))
            , test "Bad checksum" <|
                \_ ->
                    validate "1234" "1234567"
                        |> Expect.equal (Err (Account SwedishBankAccountNumber.BadChecksum))
            , test "Bad checksum Handelsbanken" <|
                \_ ->
                    validate "6234" "123456780"
                        |> Expect.equal (Err (Account SwedishBankAccountNumber.BadChecksum))
            ]
        , describe "getAccountNumberLength"
            [ test "Handelsbanken" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "6987"
                        |> Result.map (Tuple.second >> SwedishBankAccountNumber.getAccountNumberLength)
                        |> Expect.equal (Ok (SwedishBankAccountNumber.FixedLength 9))
            , test "Swedbank 4-digit" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "7123"
                        |> Result.map (Tuple.second >> SwedishBankAccountNumber.getAccountNumberLength)
                        |> Expect.equal (Ok (SwedishBankAccountNumber.FixedLength 7))
            , test "Swedbank 5-digit" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "81232"
                        |> Result.map (Tuple.second >> SwedishBankAccountNumber.getAccountNumberLength)
                        |> Expect.equal (Ok (SwedishBankAccountNumber.Range 6 10))
            ]
        ]


bankSuite : Test
bankSuite =
    -- https://github.com/jop-io/kontonummer.js/blob/7543f48bcac54a6d43fde19fba07e3404d112651/test.html
    describe "Banks"
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


clearingNumberSuite : Test
clearingNumberSuite =
    describe "ClearingNumber"
        [ describe "Error"
            [ test "Too short" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "123"
                        |> Expect.equal (Err (SwedishBankAccountNumber.ClearingNumber.BadLength 3))
            , test "Too long" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "123456"
                        |> Expect.equal (Err (SwedishBankAccountNumber.ClearingNumber.BadLength 6))
            , test "Unknown" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "9999"
                        |> Expect.equal (Err SwedishBankAccountNumber.ClearingNumber.Unknown)
            , test "Unknown Swedbank" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "81234"
                        |> Expect.equal (Err SwedishBankAccountNumber.ClearingNumber.Unknown)
            ]
        , test "toString" <|
            \_ ->
                SwedishBankAccountNumber.ClearingNumber.fromString " 96 61-"
                    |> Result.map (Tuple.second >> SwedishBankAccountNumber.ClearingNumber.toString)
                    |> Expect.equal (Ok "9661")
        , test "getBankName" <|
            \_ ->
                SwedishBankAccountNumber.ClearingNumber.fromString "9661"
                    |> Result.map (Tuple.second >> SwedishBankAccountNumber.ClearingNumber.getBankName)
                    |> Expect.equal (Ok "Svea Bank")
        , describe "Category"
            [ test "Standard" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "7000"
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok SwedishBankAccountNumber.ClearingNumber.Standard)
            , test "DataclearingOnly" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "9550"
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok SwedishBankAccountNumber.ClearingNumber.DataclearingOnly)
            , test "Historical" <|
                \_ ->
                    SwedishBankAccountNumber.ClearingNumber.fromString "9400"
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok SwedishBankAccountNumber.ClearingNumber.Historical)
            ]
        ]
