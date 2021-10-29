module PrimaFunction exposing
    ( curry, uncurry, flip
    , ifThenElse, ifThenMap, ifThenElseMap
    )

{-|

@docs curry, uncurry, flip


# Functional conditionals

@docs ifThenElse, ifThenMap, ifThenElseMap

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


{-| If function

        ifThenElse True "x" "_" -- => "x"
        ifThenElse False "_" "x" -- => "x"

-}
ifThenElse : Bool -> a -> a -> a
ifThenElse condition a b =
    if condition then
        a

    else
        b


{-| Maps the value whether the given predicate holds true for that value

        showResults : List String -> List String
        showResults xs =
            xs
                |> ifThenMap List.isEmpty
                    (\_ -> [ "Cannot find users matching this query" ])

-}
ifThenMap : (a -> Bool) -> (a -> a) -> a -> a
ifThenMap condition mapper m =
    if condition m then
        mapper m

    else
        m


{-| Conditional if that can be used in update circuits to avoid parenthesis/anonymous functions
eg.

        model
        |> doSomethingNiceWithModel
        |> ifThenElseMap someBooleanConditionBasedOnModel
            changeModelIfTrue
            changeModelIfFalse
        |> withoutCmds []

-}
ifThenElseMap : (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifThenElseMap condition mapper1 mapper2 m =
    if condition m then
        mapper1 m

    else
        mapper2 m
