module UpdateTest exposing (..)

import Expect
import Fuzz
import PrimaUpdate exposing (PrimaUpdate)
import Test exposing (..)
import TestHelpers


suite : Test
suite =
    describe "PrimaFunction tests"
        [ describe "withoutCmds"
            [ (PrimaUpdate.withoutCmds >> getModel)
                |> TestHelpers.shouldBehaveLike identity
                |> fuzz Fuzz.int "wraps the model"
            , test "does not perform side effects" <|
                \() ->
                    ()
                        |> PrimaUpdate.withoutCmds
                        |> getCmd
                        |> Expect.equal Cmd.none
            ]
        , describe "mapModel"
            [ PrimaUpdate.mapModel increment
                |> TestHelpers.shouldBehaveLike (Tuple.mapFirst increment)
                |> fuzz (fuzzUpdate Fuzz.int) "wraps the model"
            ]
        ]


{-| does not fuzz Cmd expect for Cmd.none
-}
fuzzUpdate : Fuzz.Fuzzer model -> Fuzz.Fuzzer (PrimaUpdate model (Cmd msg))
fuzzUpdate fuzzModel =
    Fuzz.tuple ( fuzzModel, Fuzz.constant Cmd.none )


increment : Int -> Int
increment =
    (+) 1


getModel : PrimaUpdate model x -> model
getModel =
    Tuple.first


getCmd : PrimaUpdate x msg -> Cmd msg
getCmd =
    Tuple.second
