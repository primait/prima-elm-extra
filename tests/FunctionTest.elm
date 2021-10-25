module FunctionTest exposing (..)

import Expect
import ExpectExtra
import Fuzz
import PrimaFunction
import Test exposing (..)


suite : Test
suite =
    describe "PrimaFunction tests"
        [ describe "(un)curry"
            [ List.member
                |> (PrimaFunction.uncurry >> PrimaFunction.curry)
                |> ExpectExtra.shouldBehaveLike2 List.member
                |> fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "(uncurry >> curry) List.member == List.member"
            ]
        , describe "flip"
            [ List.member
                |> PrimaFunction.flip
                |> ExpectExtra.shouldBehaveLike2 (\x y -> List.member y x)
                |> fuzz2 (Fuzz.list Fuzz.int) Fuzz.int "flip should flip its arguments arguments"
            ]
        , describe "ifThenElse"
            [ test "with truthy argument should return its first value" <|
                testIfthenElseWhenTrue
            , test "with falsy argument should return its second value" <|
                testIfthenElseWhenFalse
            , PrimaFunction.ifThenElse
                |> ExpectExtra.shouldBehaveLike3
                    (\b x y ->
                        if b then
                            x

                        else
                            y
                    )
                |> fuzz3 Fuzz.bool Fuzz.int Fuzz.int "should behave like an if expression"
            ]
        , describe "ifThenMap"
            [ test "with truthy argument should apply the given function" <|
                emptyString
                    >> PrimaFunction.ifThenMap String.isEmpty ((++) "!")
                    >> Expect.equal "!"
            , test "with falsy argument should not apply the given function" <|
                notEmptyString
                    >> PrimaFunction.ifThenMap String.isEmpty ((++) "!")
                    >> Expect.equal "NOT_EMPTY"
            ]
        , describe "ifThenElseMap"
            [ test "with truthy argument should apply the first function" <|
                emptyList
                    >> PrimaFunction.ifThenElseMap List.isEmpty ((::) 100) (List.take 1)
                    >> Expect.equal [ 100 ]
            , test "with falsy argument should apply the second function" <|
                notEmptyList
                    >> PrimaFunction.ifThenElseMap List.isEmpty ((::) 100) (List.take 1)
                    >> Expect.equal [ 0 ]
            , test "type regression test" <|
                num42
                    >> PrimaFunction.ifThenElseMap (always True) String.fromInt String.fromInt
                    >> Expect.equal "42"
            ]
        ]


num42 () =
    42


notEmptyList () =
    [ 0, 10, 20 ]


emptyList () =
    []


emptyString () =
    ""


notEmptyString () =
    "NOT_EMPTY"


testIfthenElseWhenTrue () =
    PrimaFunction.ifThenElse True 0 1
        |> Expect.equal 0


testIfthenElseWhenFalse () =
    PrimaFunction.ifThenElse False 0 1
        |> Expect.equal 1
