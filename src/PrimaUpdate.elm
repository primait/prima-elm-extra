module PrimaUpdate exposing
    ( PrimaUpdate
    , withCmd, andThen, withCmds, withCmdsMap, withoutCmds, mapModel, mapCmd
    )

{-| Update function helpers

@docs PrimaUpdate


# Update helpers

@docs withCmd, andThen, withCmds, withCmdsMap, withoutCmds, mapModel, mapCmd

-}


{-| The alias for a `(Model, Cmd Msg)` pair.

    init : Update Model Msg
    init =
        { users = Nothing }
            |> PrimaUpdate.withCmd fetchUsers

-}
type alias PrimaUpdate model msg =
    ( model, Cmd msg )


{-| Wraps a command and a model in a PrimaUpdate pair

    model
    |> withCmd cmd
    -- => (model, cmd)

-}
withCmd : Cmd msg -> model -> PrimaUpdate model msg
withCmd cmd model =
    ( model, cmd )


{-| Like [withCmd](PrimaUpdate#withCmd) but batches them

    model
    |> withCmds [cmd1, cmd2]
    -- => (model, Cmd.batch [cmd1, cmd2])

-}
withCmds : List (Cmd msg) -> model -> PrimaUpdate model msg
withCmds cmds model =
    ( model, Cmd.batch cmds )


{-| Used to apply an updated model to the cmds in fluid style updating

        serializeUsers : Model -> Cmd Msg
        logEvent : String -> Model -> Cmd Msg

        update : Msg -> Model -> PrimaUpdate Model Msg
        update msg model =
            case msg of
                FetchedUsers newUsers ->
                    model
                        |> setUsers newUsers
                        |> withCmdsMap [ serializeUsers, logEvent "got users" ]

-}
withCmdsMap : List (model -> Cmd msg) -> model -> PrimaUpdate model msg
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
mapModel : (model -> otherModel) -> PrimaUpdate model msg -> PrimaUpdate otherModel msg
mapModel =
    Tuple.mapFirst


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
mapCmd : (msg -> otherMsg) -> PrimaUpdate model msg -> PrimaUpdate model otherMsg
mapCmd mapper ( model, cmds ) =
    ( model, Cmd.map mapper cmds )



-- Monad instance


{-| Lifts a model to a `(model, Cmd msg)` pair
-}
withoutCmds : model -> PrimaUpdate model msg
withoutCmds model =
    ( model, Cmd.none )


{-| Concatenates Updates batching their updates

    (someModel, someCommand)
    |> andThen (\ someModel -> (anotherModel, anotherCommand))

    -- =>  (anotherModel, Cmd.batch [someCommand, anotherCommand] )

-}
andThen : (model -> PrimaUpdate otherModel msg) -> PrimaUpdate model msg -> PrimaUpdate otherModel msg
andThen f ( model, cmds ) =
    let
        ( newModel, newCmds ) =
            f model
    in
    ( newModel, Cmd.batch [ cmds, newCmds ] )
