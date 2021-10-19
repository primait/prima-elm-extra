module Prima.Extra.Time exposing (intToMaybeMonth, toEnUsFormat)

{-|

@docs intToMaybeMonth, toEnUsFormat

-}

import Time exposing (Month)


{-| -}
toEnUsFormat : String -> String
toEnUsFormat date =
    date
        |> String.toList
        |> List.filter Char.isDigit
        |> String.fromList
        |> String.split ""
        |> (\list ->
                [ (String.join "" << List.take 2 << List.drop 2) list
                , (String.join "" << List.take 2) list
                , (String.join "" << List.take 4 << List.drop 4) list
                ]
           )
        |> String.join "/"


{-| Maps a month index (within the domain [1-12] ) to a Month.

    intToMaybeMonth 1 -- => Just Time.Jan

    intToMaybeMonth 999 -- => Nothing

-}
intToMaybeMonth : Int -> Maybe Month
intToMaybeMonth int =
    case int of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing
