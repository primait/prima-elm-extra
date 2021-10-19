module Prima.Extra.String exposing (capitalize, isEmpty)

{-|

@docs capitalize, isEmpty

-}


{-| Checks whether a string is empty
-}
isEmpty : String -> Bool
isEmpty =
    (==) ""


{-|

        capitalize "hello" -- => "Hello"
        capitalize "Hello" -- => "Hello"

-}
capitalize : String -> String
capitalize str =
    (String.left 1 >> String.toUpper) str ++ String.dropLeft 1 str
