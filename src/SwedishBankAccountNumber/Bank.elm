module SwedishBankAccountNumber.Bank exposing (AccountNumberLength(..), Bank, fromClearing, getAccountNumberLength, getName, toIntList, validateAccountNumber)

{-| This file is basically Bankgirot’s PDF converted into code.
Go read it to get familiar with lingo such as “Type 1” and “Type 2”.
-}

import SwedishBankAccountNumber.Checksum as Checksum



-- HELPER FUNCTIONS


fromClearing : Int -> Maybe Bank
fromClearing clearing =
    List.filterMap
        (\( bank, matcher ) ->
            if matcher clearing then
                Just bank

            else
                Nothing
        )
        clearingRanges
        |> List.head


toIntList : String -> List Int
toIntList =
    String.toList >> List.filterMap (String.fromChar >> String.toInt)


{-| `range 5 10` returns a new function. That function checks that its parameter
is at least 5 and at most 10.
-}
range : Int -> Int -> Int -> Bool
range minimum maximum clearing =
    clearing >= minimum && clearing <= maximum


{-| Combine several `range`s. Returns `True` if any of them match.
-}
combine : List (Int -> Bool) -> Int -> Bool
combine validators clearing =
    List.any (\matcher -> matcher clearing) validators



-- VALIDATION


type AccountNumberLength
    = FixedLength Int
    | Range Int Int


type AccountType
    = Type1 AccountType1Validation
    | Type2 AccountType2Validation


type AccountType1Validation
    = Full
    | SkipFirstDigit


type AccountType2Validation
    = Mod10
    | Mod11


validateAccountNumber : Bank -> List Int -> List Int -> Bool
validateAccountNumber bank clearingInts accountInts =
    -- Bankgirot’s PDF has marked all banks/clearing numbers with a “comment” –
    -- 1, 2 and 3 – that describe how to validate.
    case getAccountType bank of
        -- Type 1, Comment 2
        Type1 Full ->
            Checksum.mod11 (clearingInts ++ accountInts)

        -- Type 1, Comment 1
        Type1 SkipFirstDigit ->
            Checksum.mod11 (List.drop 1 clearingInts ++ accountInts)

        -- Type 2, Comment 1/3
        Type2 Mod10 ->
            Checksum.mod10 accountInts

        -- Type 2, Comment 2
        Type2 Mod11 ->
            Checksum.mod11 accountInts



-- BANK DATA


{-| Some banks have different validation rules for different clearing number
ranges, which is why some of the variants below carry extra data.
-}
type Bank
    = Svea
    | Avanza
    | BlueStep
    | BNP
    | Citibank
    | DNB
    | Ekobanken
    | ErikPenser
    | Forex
    | ICA
    | IKANO
    | JAK
    | Klarna
    | Landshypotek
    | LanSpar
    | Lansforsakringar AccountType1Validation
    | Marginalen
    | MedMera
    | Nordax
    | Nordnet
    | Resurs
    | Santander
    | SBAB
    | SEB
    | Skandia
    | Alandsbanken
    | Danske AccountType
    | Handelsbanken
    | Nordea AccountType1Validation
    | NordeaPersonkonto
    | NordeaPlusgirot
    | Riksgalden AccountType
    | SparbankenSyd
    | Swedbank AccountType
    | SwedbankSparbankenOresund


getName : Bank -> String
getName bank =
    case bank of
        Svea ->
            "Svea Bank"

        Avanza ->
            "Avanza Bank"

        BNP ->
            "BNP Paribas SA."

        Citibank ->
            "Citibank"

        BlueStep ->
            "BlueStep Finans"

        DNB ->
            "DNB Bank"

        Ekobanken ->
            "Ekobanken"

        ErikPenser ->
            "Erik Penser"

        Forex ->
            "Forex Bank"

        ICA ->
            "ICA Banken"

        IKANO ->
            "IKANO Bank"

        JAK ->
            "JAK Medlemsbank"

        Klarna ->
            "Klarna Bank"

        Landshypotek ->
            "Landshypotek"

        LanSpar ->
            "Lån & Spar Bank Sverige"

        Lansforsakringar _ ->
            "Länsförsäkringar Bank"

        Marginalen ->
            "Marginalen Bank"

        MedMera ->
            "MedMera Bank"

        Nordax ->
            "Nordax Bank"

        Nordnet ->
            "Nordnet Bank"

        Resurs ->
            "Resurs Bank"

        Santander ->
            "Santander Consumer Bank"

        SBAB ->
            "SBAB"

        SEB ->
            "SEB"

        Skandia ->
            "Skandiabanken"

        Alandsbanken ->
            "Ålandsbanken"

        Danske _ ->
            "Danske Bank"

        Handelsbanken ->
            "Handelsbanken"

        Nordea _ ->
            "Nordea"

        NordeaPersonkonto ->
            "Nordea"

        NordeaPlusgirot ->
            "Nordea Plusgirot"

        Riksgalden _ ->
            "Riksgälden"

        SparbankenSyd ->
            "Sparbanken Syd"

        Swedbank _ ->
            "Swedbank"

        SwedbankSparbankenOresund ->
            -- Sparbanken Öresund was bought by Swedbank in 2014.
            "Swedbank"


clearingRanges : List ( Bank, Int -> Bool )
clearingRanges =
    [ ( Svea
      , range 9660 9669
      )
    , ( Avanza
      , range 9550 9569
      )
    , ( BlueStep
      , range 9680 9689
      )
    , ( BNP
      , range 9470 9479
      )
    , ( Citibank
      , range 9040 9049
      )
    , ( DNB
      , combine
            [ range 9190 9199
            , range 9260 9269
            ]
      )
    , ( Ekobanken
      , range 9700 9709
      )
    , ( ErikPenser
      , range 9590 9599
      )
    , ( Forex
      , range 9400 9449
      )
    , ( ICA
      , range 9270 9279
      )
    , ( IKANO
      , range 9170 9179
      )
    , ( JAK
      , range 9670 9679
      )
    , ( Klarna
      , range 9780 9789
      )
    , ( Landshypotek
      , range 9390 9399
      )
    , ( LanSpar
      , range 9630 9639
      )
    , ( Lansforsakringar SkipFirstDigit
      , combine
            [ range 3400 3409
            , range 9060 9069
            ]
      )
    , ( Lansforsakringar Full
      , range 9020 9029
      )
    , ( Marginalen
      , range 9230 9239
      )
    , ( MedMera
      , range 9650 9659
      )
    , ( Nordax
      , range 9640 9649
      )
    , ( Nordnet
      , range 9100 9109
      )
    , ( Resurs
      , range 9280 9289
      )
    , ( Santander
      , range 9460 9469
      )
    , ( SBAB
      , range 9250 9259
      )
    , ( SEB
      , combine
            [ range 5000 5999
            , range 9120 9124
            , range 9130 9149
            ]
      )
    , ( Skandia
      , range 9150 9169
      )
    , ( Alandsbanken
      , range 2300 2399
      )
    , ( Danske (Type1 SkipFirstDigit)
      , combine
            [ range 1200 1399
            , range 2400 2499
            ]
      )
    , ( Danske (Type2 Mod10)
      , range 9180 9189
      )
    , ( Handelsbanken
      , range 6000 6999
      )
    , ( Nordea SkipFirstDigit
      , combine
            [ range 1100 1199
            , range 1400 2099
            , range 3000 3299
            , range 3301 3399
            , range 3410 3781
            , range 3783 3999
            ]
      )
    , ( Nordea Full
      , range 4000 4999
      )
    , ( NordeaPersonkonto
      , \clearing -> clearing == 3300 || clearing == 3782
      )
    , ( NordeaPlusgirot
      , combine
            [ range 9500 9549
            , range 9960 9969
            ]
      )
    , ( Riksgalden (Type1 Full)
      , range 9880 9889
      )
    , ( Riksgalden (Type2 Mod10)
      , range 9890 9899
      )
    , ( SparbankenSyd
      , range 9570 9579
      )
    , ( Swedbank (Type1 SkipFirstDigit)
      , range 7000 7999
      )
    , ( Swedbank (Type2 Mod10)
      , \clearing ->
            range 80000 89999 clearing
                && Checksum.mod10 (toIntList (String.fromInt clearing))
      )
    , ( SwedbankSparbankenOresund
      , combine
            [ range 9300 9329
            , range 9330 9349
            ]
      )
    ]


getAccountNumberLength : Bank -> AccountNumberLength
getAccountNumberLength bank =
    case bank of
        Svea ->
            FixedLength 7

        Avanza ->
            FixedLength 7

        BlueStep ->
            FixedLength 7

        BNP ->
            FixedLength 7

        Citibank ->
            FixedLength 7

        DNB ->
            FixedLength 7

        Ekobanken ->
            FixedLength 7

        ErikPenser ->
            FixedLength 7

        Forex ->
            FixedLength 7

        ICA ->
            FixedLength 7

        IKANO ->
            FixedLength 7

        JAK ->
            FixedLength 7

        Klarna ->
            FixedLength 7

        Landshypotek ->
            FixedLength 7

        LanSpar ->
            FixedLength 7

        Lansforsakringar _ ->
            FixedLength 7

        Marginalen ->
            FixedLength 7

        MedMera ->
            FixedLength 7

        Nordax ->
            FixedLength 7

        Nordnet ->
            FixedLength 7

        Resurs ->
            FixedLength 7

        Santander ->
            FixedLength 7

        SBAB ->
            FixedLength 7

        SEB ->
            FixedLength 7

        Skandia ->
            FixedLength 7

        Alandsbanken ->
            FixedLength 7

        Danske (Type1 _) ->
            FixedLength 7

        Danske (Type2 _) ->
            FixedLength 10

        Handelsbanken ->
            FixedLength 9

        Nordea _ ->
            FixedLength 7

        NordeaPersonkonto ->
            FixedLength 10

        NordeaPlusgirot ->
            Range 7 10

        Riksgalden (Type1 _) ->
            FixedLength 7

        Riksgalden (Type2 _) ->
            FixedLength 10

        SparbankenSyd ->
            FixedLength 10

        Swedbank (Type1 _) ->
            FixedLength 7

        Swedbank (Type2 _) ->
            Range 6 10

        SwedbankSparbankenOresund ->
            FixedLength 10


getAccountType : Bank -> AccountType
getAccountType bank =
    case bank of
        Svea ->
            Type1 Full

        Avanza ->
            Type1 Full

        BlueStep ->
            Type1 SkipFirstDigit

        BNP ->
            Type1 Full

        Citibank ->
            Type1 Full

        DNB ->
            Type1 Full

        Ekobanken ->
            Type1 Full

        ErikPenser ->
            Type1 Full

        Forex ->
            Type1 SkipFirstDigit

        ICA ->
            Type1 SkipFirstDigit

        IKANO ->
            Type1 SkipFirstDigit

        JAK ->
            Type1 Full

        Klarna ->
            Type1 Full

        Landshypotek ->
            Type1 Full

        LanSpar ->
            Type1 SkipFirstDigit

        Lansforsakringar type1validation ->
            Type1 type1validation

        Marginalen ->
            Type1 SkipFirstDigit

        MedMera ->
            Type1 Full

        Nordax ->
            Type1 Full

        Nordnet ->
            Type1 Full

        Resurs ->
            Type1 SkipFirstDigit

        Santander ->
            Type1 SkipFirstDigit

        SBAB ->
            Type1 SkipFirstDigit

        SEB ->
            Type1 SkipFirstDigit

        Skandia ->
            Type1 Full

        Alandsbanken ->
            Type1 Full

        Danske accountType ->
            accountType

        Handelsbanken ->
            Type2 Mod11

        Nordea type1validation ->
            Type1 type1validation

        NordeaPersonkonto ->
            Type2 Mod10

        NordeaPlusgirot ->
            Type2 Mod10

        Riksgalden accountType ->
            accountType

        SparbankenSyd ->
            Type2 Mod10

        Swedbank accountType ->
            accountType

        SwedbankSparbankenOresund ->
            Type2 Mod10
