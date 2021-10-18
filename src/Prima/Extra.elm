module Prima.Extra exposing
    ( ifThenElse
    , ifThenElseCmds
    , ifThenElseMap
    , ifThenMap
    )


ifThenElseCmds : Bool -> List (Cmd msg) -> List (Cmd msg) -> Cmd msg
ifThenElseCmds condition cmds1 cmds2 =
    if condition then
        Cmd.batch cmds1

    else
        Cmd.batch cmds2


ifThenElse : Bool -> anything -> anything -> anything
ifThenElse condition a b =
    if condition then
        a

    else
        b


ifThenMap : (m -> Bool) -> (m -> m) -> m -> m
ifThenMap condition mapper m =
    if condition m then
        mapper m

    else
        m



{--| conditional if that can be used in update circuits to avoid parenthesis/anonymous functions
eg.
  model
    |> doSomethingNiceWithModel
    |> ifThenElseMap someBooleanConditionBasedOnModel
        changeModelIfTrue
        changeModelIfFalse
    |> withoutCmds []
-}


ifThenElseMap : (m -> Bool) -> (m -> a) -> (m -> a) -> m -> a
ifThenElseMap condition mapper1 mapper2 m =
    if condition m then
        mapper1 m

    else
        mapper2 m
