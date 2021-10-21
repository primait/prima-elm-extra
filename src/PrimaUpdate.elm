module PrimaUpdate exposing
    ( Update
    , withCmd, andThen, withCmds, withCmdsMap, withoutCmds, mapModel, mapCmd, getModel, getCmd
    )

{-| Update function helpers

@docs Update


# Update helpers

@docs withCmd, andThen, withCmds, withCmdsMap, withoutCmds, mapModel, mapCmd, getModel, getCmd

-}


{-| The alias for a `(Model, Cmd Msg)` pair.

    init : Update Model Msg
    init =
        { users = Nothing }
            |> PrimaUpdate.withCmd fetchUsers

-}



-- TODO need this comment for the drone build to fail (via elm-analyse)


type alias Update model msg =
    ( model, Cmd msg )


{-| Alias for Tuple.first
-}
getModel : Update model x -> model
getModel =
    Tuple.first


{-| Alias for Tuple.second
-}
getCmd : Update x msg -> Cmd msg
getCmd =
    Tuple.second


{-| -}
withCmd : Cmd msg -> model -> Update model msg
withCmd cmd model =
    ( model, cmd )


{-| -}
withCmds : List (Cmd msg) -> model -> Update model msg
withCmds cmds model =
    ( model, Cmd.batch cmds )


{-| Used to apply an updated model to the cmds in fluid style updating
-}
withCmdsMap : List (model -> Cmd msg) -> model -> Update model msg
withCmdsMap cmdFunctions model =
    ( model
    , cmdFunctions
        |> List.map (\f -> f model)
        |> Cmd.batch
    )



-- BiFunctor instance


{-| Maps the model preserving the current cmds

     { count = 0 }
        |> PrimaCmd.withoutCmds
        |> PrimaCmd.mapModel (updateCount ((+) 1))
        |> Expect.equal ({ count = 1 }, Cmd.none)

-}
mapModel : (model -> otherModel) -> Update model msg -> Update otherModel msg
mapModel mapper ( model, cmds ) =
    ( mapper model, cmds )


{-| `Cmd.map`s the pair cmd, preserving the model

    intCmd : Cmd Int
    intCmd =
        Task.succeed 42 |> Task.perform identity

    pair : Update () String
    pair =
        ()
            |> withCmd intCmd
            |> mapCmd String.fromInt

-}
mapCmd : (msg -> otherMsg) -> Update model msg -> Update model otherMsg
mapCmd mapper ( model, cmds ) =
    ( model, Cmd.map mapper cmds )



-- Monad instance


{-| Lifts a model to a `(model, Cmd msg)` pair
-}
withoutCmds : model -> Update model msg
withoutCmds model =
    ( model, Cmd.none )


{-| Concatenates Updates batching their updates

    (someModel, someCommand)
    |> andThen (\ someModel -> (anotherModel, anotherCommand))

    -- =>  (anotherModel, Cmd.batch [someCommand, anotherCommand] )

-}
andThen : (model -> Update otherModel msg) -> Update model msg -> Update otherModel msg
andThen f ( model, cmds ) =
    let
        ( newModel, newCmds ) =
            f model
    in
    ( newModel, Cmd.batch [ cmds, newCmds ] )
