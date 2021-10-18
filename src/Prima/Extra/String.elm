module Prima.Extra.String exposing
    ( capitalize
    , isEmpty
    )


isEmpty : String -> Bool
isEmpty =
    (==) ""


capitalize : String -> String
capitalize str =
    (String.left 1 >> String.toUpper) str ++ String.dropLeft 1 str
