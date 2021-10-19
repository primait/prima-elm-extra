module Prima.Extra exposing (ifThenMap, ifThenElse, ifThenElseMap)

{-|


# Conditionals

@docs ifThenMap, ifThenElse, ifThenElseMap

-}


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
