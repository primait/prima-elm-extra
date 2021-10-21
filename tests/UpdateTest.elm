module UpdateTest exposing (..)

import Expect
import Fuzz
import PrimaUpdate exposing (Update)
import Test exposing (..)
import TestHelpers


suite : Test
suite =
    describe "PrimaFunction tests"
        [ describe "withoutCmds"
            [ fuzz Fuzz.int "wraps the model" <|
                ((PrimaUpdate.withoutCmds >> PrimaUpdate.getModel)
                    |> TestHelpers.shouldBehaveLike identity
                )
            , test "does not perform side effects" <|
                \() ->
                    ()
                        |> PrimaUpdate.withoutCmds
                        |> PrimaUpdate.getCmd
                        |> Expect.equal Cmd.none
            ]
        , describe "mapModel"
            [ fuzz (fuzzUpdate Fuzz.int) "wraps the model" <|
                (PrimaUpdate.mapModel increment
                    |> TestHelpers.shouldBehaveLike (Tuple.mapFirst increment)
                )
            ]
        ]


{-| does not fuzz Cmd expect for Cmd.none
-}
fuzzUpdate : Fuzz.Fuzzer model -> Fuzz.Fuzzer (Update model (Cmd msg))
fuzzUpdate fuzzModel =
    Fuzz.tuple ( fuzzModel, Fuzz.constant Cmd.none )


increment : Int -> Int
increment =
    (+) 1
