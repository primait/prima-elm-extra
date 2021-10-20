module PrimaUpdate exposing
    ( Update
    , withCmd, andThen, withCmds, withCmdsMap, withoutCmds, mapModel, mapCmd
    )

{-| Update function helpers

@docs Update


# Update helpers

@docs withCmd, andThen, withCmds, withCmdsMap, withoutCmds, mapModel, mapCmd

-}


{-| The alias for a `(Model, Cmd Msg)` pair.

    init : Update Model Msg
    init =
        { users = Nothing }
            |> PrimaUpdate.withCmd fetchUsers

-}
type alias Update model msg =
    ( model, Cmd msg )


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
    ( model, Cmd.batch <| List.map (\fun -> fun model) cmdFunctions )



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
{- Proof: (TODO double check)

   1. withoutCmds model |> andThen f =?? f model

        withoutCmds model |> andThen f

        ==

        (model, Cmd.none) |> andThen f

        ==

        let (newModel, newCmd) = f model
        in (newModel, Cmd.batch [Cmd.none, newCmd])

        ==

        let (newModel, newCmd) = f model
        in (newModel, newCmd)

        ==

        f model (OK)

    2. (model, cmd) |> andThen withoutCmds =?? (model, cmd)

        (model, cmd) |> andThen withoutCmds

        ==

        let (newModel, newCmd) = withoutCmds model
        in (newModel, Cmd.batch [cmd, newCmd])

        ==

        (model, Cmd.batch [cmd, Cmd.none])

        ==

        (model, cmd) (OK)

    3.  (m, cmd) |> andThen (\m -> f m |> andThen g) =?? (m, cmd) |> andThen f |> andThen g


        a) (m, cmd) |> andThen (\m_ -> f m_ |> andThen g)

        ==

        let ( m1, cmd1 ) = (\m_ -> f m_ |> andThen g) m in
        ( m1, Cmd.batch [ cmd, cmd1 ] )

        ==

        let ( m1, cmd1 ) = f m |> andThen g in
        ( m1, Cmd.batch [ cmd, cmd1 ] )

        ==

        let ( m1, cmd1 ) = (
            let (m2, cmd2) = f m in
            (m2, cmd2) |> andThen g
        ) in
        ( m1, Cmd.batch [ cmd, cmd1 ] )

        ==

        let ( m1, cmd1 ) = (
            let (m2, cmd2) = f m in
            let (m3, cmd3) = g m2 in
            (m3, Cmd.batch [ cmd2, cmd3 ])
        ) in
        ( m1, Cmd.batch [ cmd, cmd1 ] )

        ==

        let (m2, cmd2) = f m in
        let (m3, cmd3) = g m2 in
        let ( m1, cmd1 ) =  (m3, Cmd.batch [ cmd2, cmd3 ]) in
        ( m1, Cmd.batch [ cmd, cmd1 ] )

        ==

        let (m2, cmd2) = f m in
        let (m3, cmd3) = g m2 in
        ( m3, Cmd.batch [ cmd, Cmd.batch [ cmd2, cmd3 ]] )

        ==

        let (m2, cmd2) = f m in
        let (m3, cmd3) = g m2 in
        ( m3, Cmd.batch [ cmd, cmd2, cmd3 ] )


        b) ((m, cmd) |> andThen f) |> andThen g ==

        (let (m1, cmd1) = f m
         in (m1, Cmd.batch [cmd, cmd1]))
        |> andThen g

        ==

        let (m1, cmd1) = f m in
        let (m2, cmd2) = g m1 in
        in (m2, Cmd.batch [Cmd.batch [cmd, cmd1], cmd2]

        ==

        let (m1, cmd1) = f m in
        let (m2, cmd2) = g m1 in
        in (m2, Cmd.batch [cmd, cmd1, cmd2])  (OK)

-}


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
