module Prima.Extra.Float exposing (takePriceDecimalPart)

{-|

@docs takePriceDecimalPart

-}


{-| -}
takePriceDecimalPart : Float -> Int
takePriceDecimalPart float =
    modBy 100 (round (float * 100))
