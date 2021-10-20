module PrimaCmd exposing
    ( batchMap, cmdMap
    , ifThenCmd, ifThenCmdMap, ifThenCmds, ifThenCmdsMap, ifThenElseCmdMap, ifThenElseCmdsMap, ifThenElseCmds
    , delayMsg, toCmd
    )

{-|

@docs batchMap, cmdMap


# Conditionals

@docs ifThenCmd, ifThenCmdMap, ifThenCmds, ifThenCmdsMap, ifThenElseCmdMap, ifThenElseCmdsMap, ifThenElseCmds


# Effects

@docs delayMsg, toCmd

-}

import Process
import Task


{-| Lifts a msg to a Cmd.

**warning:** this is usually an antipattern.

There are specific case in which this can be used
(such as [flip](https://css-tricks.com/animating-layouts-with-the-flip-technique/) animations) but
unless you encountered such edge cases, be sure to double check if there are different approaches possible

-}
toCmd : msg -> Cmd msg
toCmd =
    Task.perform identity << Task.succeed


{-| Create a Cmd that triggers the given msg after n milliseconds
-}
delayMsg : Int -> msg -> Cmd msg
delayMsg millis msg =
    Process.sleep (toFloat millis)
        |> Task.andThen (Task.succeed msg |> always)
        |> Task.perform identity


{-| -}
ifThenCmd : Bool -> Cmd msg -> Cmd msg
ifThenCmd condition cmd =
    if condition then
        cmd

    else
        Cmd.none


{-| -}
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


{-| -}
batchMap : List (a -> Cmd msg) -> a -> Cmd msg
batchMap cmds a =
    a
        |> cmdMap cmds
        |> Cmd.batch


{-| -}
ifThenCmdMap : (a -> Bool) -> (a -> Cmd msg) -> a -> Cmd msg
ifThenCmdMap condition cmd a =
    ifThenCmd (condition a) (cmd a)


{-| -}
ifThenCmdsMap : (a -> Bool) -> List (a -> Cmd msg) -> a -> Cmd msg
ifThenCmdsMap condition cmdList a =
    a
        |> batchMap cmdList
        |> ifThenCmd (condition a)


{-| -}
ifThenElseCmdMap : (a -> Bool) -> (a -> Cmd msg) -> (a -> Cmd msg) -> a -> Cmd msg
ifThenElseCmdMap condition cmd1 cmd2 a =
    if condition a then
        cmd1 a

    else
        cmd2 a


{-| -}
ifThenElseCmdsMap : (a -> Bool) -> List (a -> Cmd msg) -> List (a -> Cmd msg) -> a -> Cmd msg
ifThenElseCmdsMap condition cmds1 cmds2 a =
    -- this if is redundant but avoids to execute eventual Debug.log functions inside command list
    if condition a then
        ifThenElseCmds (condition a) (cmdMap cmds1 a) []

    else
        ifThenElseCmds (condition a) [] (cmdMap cmds2 a)


{-| -}
ifThenElseCmds : Bool -> List (Cmd msg) -> List (Cmd msg) -> Cmd msg
ifThenElseCmds condition cmds1 cmds2 =
    if condition then
        Cmd.batch cmds1

    else
        Cmd.batch cmds2
