module SwedishBankAccountNumber.Checksum exposing (mod10, mod11)

{-| Implementations of “10-modulmetoden” and “11-modulmetoden” as described in
Bankgirot’s PDF.
-}


mod10 : List Int -> Bool
mod10 ints =
    ints
        |> List.reverse
        |> List.indexedMap
            (\index num ->
                let
                    product =
                        cycleFrom1ToX 2 index * num
                in
                if product >= 10 then
                    product - 9

                else
                    product
            )
        |> List.sum
        |> modBy 10
        |> (==) 0


mod11 : List Int -> Bool
mod11 ints =
    ints
        |> List.reverse
        |> List.indexedMap
            (\index num ->
                cycleFrom1ToX 10 index * num
            )
        |> List.sum
        |> modBy 11
        |> (==) 0


{-| Examples:

    List.range 0 13 |> List.map (cycleFrom1ToX 2)
    --> [1,2,1,2,1,2,1,2,1,2,1,2,1,2]

    List.range 0 13 |> List.map (cycleFrom1ToX 10)
    --> [1,2,3,4,5,6,7,8,9,10,1,2,3,4]

-}
cycleFrom1ToX : Int -> Int -> Int
cycleFrom1ToX x index =
    (index |> modBy x) + 1
