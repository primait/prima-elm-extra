module Prima.Extra.Function exposing (curry, uncurry, flip)

{-|

@docs curry, uncurry, flip

-}


{-| Given a function with a single argument as pair, returns its curried version

        sumPair : (Int, Int) -> Int -> Int
        sumPair (x, y) = x + y

        curriedSum : Int -> Int -> Int
        curriedSum x y = curry sumPair

-}
curry : (( a, b ) -> c) -> a -> b -> c
curry f x y =
    f ( x, y )


{-| Given a function with two (curried) arguments, returns a function whose only argument is a pair

        curriedSum : Int -> Int -> Int
        curriedSum x y = x + y

        sumPair : (Int, Int) -> Int -> Int
        sumPair = uncurry curriedSum

-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y


{-| Flips the first two (curried) arguments of a function

        cons : a -> List a -> List a
        cons = (::)

        flippedCons : List a -> a -> List a
        flippedCons = flip cons

-}
flip : (a -> b -> c) -> b -> a -> c
flip mapper b a =
    mapper a b
