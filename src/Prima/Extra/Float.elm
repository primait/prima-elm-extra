module Prima.Extra.Float exposing
    ( takePriceDecimalPart
    , takePriceIntegerPart
    )


takePriceDecimalPart : Float -> Int
takePriceDecimalPart float =
    modBy 100 (round (float * 100))



-- ??


takePriceIntegerPart : Float -> Int
takePriceIntegerPart price =
    floor price
