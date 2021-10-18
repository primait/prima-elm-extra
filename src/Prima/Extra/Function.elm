module Prima.Extra.Function exposing (curry, uncurry, flip)

{-|

@docs curry, uncurry, flip

-}


{-| test docs
-}
curry : (( a, b ) -> c) -> a -> b -> c
curry f x y =
    f ( x, y )


{-| test docs
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y


{-| test docs
-}
flip : (a -> b -> c) -> b -> a -> c
flip mapper b a =
    mapper a b
