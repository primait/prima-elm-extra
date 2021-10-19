module Prima.Extra.String exposing (capitalize)

{-|

@docs capitalize

-}


{-|

        capitalize "hello" -- => "Hello"
        capitalize "Hello" -- => "Hello"

-}
capitalize : String -> String
capitalize str =
    (String.left 1 >> String.toUpper) str ++ String.dropLeft 1 str
