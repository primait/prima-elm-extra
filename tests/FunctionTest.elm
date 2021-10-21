module FunctionTest exposing (..)

import Expect
import Fuzz
import PrimaFunction
import Test exposing (..)
import TestHelpers


suite : Test
suite =
    describe "PrimaFunction tests"
        [ describe "(un)curry"
            [ fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "(uncurry >> curry) List.member == List.member" <|
                (List.member
                    |> (PrimaFunction.uncurry >> PrimaFunction.curry)
                    |> TestHelpers.shouldBehaveLike2 List.member
                )
            ]
        , describe "flip"
            [ fuzz2 (Fuzz.list Fuzz.int) Fuzz.int "flip should flip its arguments arguments" <|
                (List.member
                    |> PrimaFunction.flip
                    |> TestHelpers.shouldBehaveLike2 (\x y -> List.member y x)
                )
            ]
        , describe "ifThenElse"
            [ test "with truthy argument should return its first value" <|
                \() ->
                    PrimaFunction.ifThenElse True 0 1
                        |> Expect.equal 0
            , test "with falsy argument should return its second value" <|
                \() ->
                    PrimaFunction.ifThenElse False 0 1
                        |> Expect.equal 1
            , fuzz3 Fuzz.bool Fuzz.int Fuzz.int "should behave like an if expression" <|
                (PrimaFunction.ifThenElse
                    |> TestHelpers.shouldBehaveLike3
                        (\b x y ->
                            if b then
                                x

                            else
                                y
                        )
                )
            ]
        , describe "ifThenMap"
            [ test "with truthy argument should apply the given function" <|
                \() ->
                    PrimaFunction.ifThenMap String.isEmpty ((++) "!") ""
                        |> Expect.equal "!"
            , test "with falsy argument should not apply the given function" <|
                \() ->
                    PrimaFunction.ifThenMap String.isEmpty ((++) "!") "NOT_EMPTY"
                        |> Expect.equal "NOT_EMPTY"
            ]
        , describe "ifThenElseMap"
            [ test "with truthy argument should apply the first function" <|
                \() ->
                    PrimaFunction.ifThenElseMap List.isEmpty ((::) 100) (List.take 1) []
                        |> Expect.equal [ 100 ]
            , test "with falsy argument should apply the second function" <|
                \() ->
                    PrimaFunction.ifThenElseMap List.isEmpty ((::) 100) (List.take 1) [ 0, 10, 20 ]
                        |> Expect.equal [ 0 ]
            ]
        ]
