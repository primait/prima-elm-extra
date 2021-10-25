module PrimaCmd exposing
    ( batchMap, cmdMap, fromMaybeMap
    , ifThenCmd, ifThenCmdMap, ifThenCmds, ifThenCmdsMap, ifThenElseCmdMap, ifThenElseCmdsMap, ifThenElseCmds
    , fromMsgWithDelay, fromMsg
    )

{-|

@docs batchMap, cmdMap, fromMaybeMap


# Conditionals

@docs ifThenCmd, ifThenCmdMap, ifThenCmds, ifThenCmdsMap, ifThenElseCmdMap, ifThenElseCmdsMap, ifThenElseCmds


# Effects

@docs fromMsgWithDelay, fromMsg

-}

import PrimaFunction
import Process
import Task


{-| Lifts a msg to a Cmd.

**warning:** this is usually an antipattern.

There are specific case in which this can be used
(such as [flip](https://css-tricks.com/animating-layouts-with-the-flip-technique/) animations) but
unless you encountered such edge cases, be sure to double check if there are different approaches possible

-}
fromMsg : msg -> Cmd msg
fromMsg =
    Task.perform identity << Task.succeed


{-| Create a Cmd that triggers the given msg after n milliseconds
-}
fromMsgWithDelay : Int -> msg -> Cmd msg
fromMsgWithDelay millis msg =
    Process.sleep (toFloat millis)
        |> Task.andThen (Task.succeed msg |> always)
        |> Task.perform identity


{-|

       ifThenCmd True x -- => x
       ifThenCmd False x -- => Cmd.none

-}
ifThenCmd : Bool -> Cmd msg -> Cmd msg
ifThenCmd condition cmd =
    if condition then
        cmd

    else
        Cmd.none


{-|

       ifThenCmd True cmds -- => Cmd.batch cmds
       ifThenCmd False x -- => Cmd.none

-}
ifThenCmds : Bool -> List (Cmd msg) -> Cmd msg
ifThenCmds condition cmds =
    if condition then
        Cmd.batch cmds

    else
        Cmd.none


{-| Maps the given Cmd suppliers to the same value. Usually used like that:

    fetchUsers : Model -> Cmd Msg
    sendLog : Model -> Cmd Msg
    initializeAnalytics : Model -> Cmd Msg

    model
        |> cmdMap
            [ fetchUsers
            , sendLog
            , initializeAnalytics
            ]

-}
cmdMap : List (a -> Cmd msg) -> a -> List (Cmd msg)
cmdMap cmds a =
    cmds
        |> List.map (\cmdFunct -> cmdFunct a)


{-| Batches the commands returned by the given functions

    fetchUsers : Model -> Cmd Msg
    serializeData : Model -> Cmd Msg

    model
    |> batchMap [ fetchUsers, serializeData ]

-}
batchMap : List (a -> Cmd msg) -> a -> Cmd msg
batchMap cmds =
    cmdMap cmds >> Cmd.batch


{-|

        fetchUsers : Model -> Cmd Msg

        model
        |> ifThenCmdMap (\model -> Maybe.isNothing model.users) fetchUsers

-}
ifThenCmdMap : (a -> Bool) -> (a -> Cmd msg) -> a -> Cmd msg
ifThenCmdMap condition cmd a =
    ifThenCmd (condition a) (cmd a)


{-| Like [`ifThenCmdMap`](PrimaCmd#ifThenCmdMap), but with a list of cmds
-}
ifThenCmdsMap : (a -> Bool) -> List (a -> Cmd msg) -> a -> Cmd msg
ifThenCmdsMap condition cmdList a =
    a
        |> batchMap cmdList
        |> ifThenCmd (condition a)


{-| Same as [`PrimaFunction.ifThenElseMap`](PrimaFunction#ifThenElseMap) but for Cmds
-}
ifThenElseCmdMap : (a -> Bool) -> (a -> Cmd msg) -> (a -> Cmd msg) -> a -> Cmd msg
ifThenElseCmdMap =
    PrimaFunction.ifThenElseMap


{-| Like [`ifThenCmdMap`](PrimaCmd#ifThenCmdMap), but for List of commands
-}
ifThenElseCmdsMap : (a -> Bool) -> List (a -> Cmd msg) -> List (a -> Cmd msg) -> a -> Cmd msg
ifThenElseCmdsMap condition cmds1 cmds2 a =
    Cmd.batch <|
        if condition a then
            cmdMap cmds1 a

        else
            cmdMap cmds2 a


{-| Like [`ifThenElse`](PrimaFunction#ifThenElse), but batches Cmds
-}
ifThenElseCmds : Bool -> List (Cmd msg) -> List (Cmd msg) -> Cmd msg
ifThenElseCmds condition cmds1 cmds2 =
    if condition then
        Cmd.batch cmds1

    else
        Cmd.batch cmds2


{-| Returns `Cmd.none` when argument is Nothing, maps it otherwise
-}
fromMaybeMap : (a -> Cmd msg) -> Maybe a -> Cmd msg
fromMaybeMap command maybe =
    case maybe of
        Just a ->
            command a

        Nothing ->
            Cmd.none
