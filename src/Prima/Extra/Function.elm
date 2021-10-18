module Prima.Extra.Function exposing
    ( curry
    , flip
    , uncurry
    )


curry : (( a, b ) -> c) -> a -> b -> c
curry f x y =
    f ( x, y )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y


flip : (a -> b -> c) -> b -> a -> c
flip mapper b a =
    mapper a b
