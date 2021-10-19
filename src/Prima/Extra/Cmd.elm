module Prima.Extra.Cmd exposing
    ( batchMap
    , cmdMap
    , mapAfterCmds
    , mapCmds
    , sendCmdMsg
    , withChildrenCmds
    , withCmd
    , withCmds
    , withCmdsMap
    , withoutCmds
    , ifThenCmd
    , ifThenCmdMap
    , ifThenCmds
    , ifThenCmdsMap
    , ifThenElseCmdMap
    , ifThenElseCmdsMap
    , ifThenElseCmds
    , delayMsg
    )

{-| Update function helpers

@docs batchMap
@docs cmdMap
@docs mapAfterCmds
@docs mapCmds
@docs sendCmdMsg
@docs withChildrenCmds
@docs withCmd
@docs withCmds
@docs withCmdsMap
@docs withoutCmds


# Conditionals

@docs ifThenCmd
@docs ifThenCmdMap
@docs ifThenCmds
@docs ifThenCmdsMap
@docs ifThenElseCmdMap
@docs ifThenElseCmdsMap
@docs ifThenElseCmds


# Effects

@docs delayMsg

-}

import Prima.Extra
import Process
import Task


{-| Wraps a msg within a Cmd
-}
sendCmdMsg : msg -> Cmd msg
sendCmdMsg =
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


{-| -}
withCmd : Cmd msg -> model -> ( model, Cmd msg )
withCmd cmd model =
    ( model, cmd )


{-| -}
withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


{-| -}
withoutCmds : model -> ( model, Cmd msg )
withoutCmds model =
    ( model, Cmd.none )


{-| -}
withChildrenCmds : (childrenMsg -> msg) -> ( model, Cmd childrenMsg ) -> ( model, Cmd msg )
withChildrenCmds mapper ( model, childrenCmd ) =
    ( model, Cmd.map mapper childrenCmd )


{-| Used to apply an updated model to the cmds in fluid style updating
-}
withCmdsMap : List (model -> Cmd msg) -> model -> ( model, Cmd msg )
withCmdsMap cmdFunctions model =
    ( model, Cmd.batch <| List.map (\fun -> fun model) cmdFunctions )


{-| -}
mapAfterCmds : (model -> model) -> ( model, Cmd msg ) -> ( model, Cmd msg )
mapAfterCmds mapper ( model, cmds ) =
    ( mapper model, cmds )


{-| -}
mapCmds : (msg -> otherMsg) -> ( model, Cmd msg ) -> ( model, Cmd otherMsg )
mapCmds mapper ( model, cmds ) =
    ( model, Cmd.map mapper cmds )
