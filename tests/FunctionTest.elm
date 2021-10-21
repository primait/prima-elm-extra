module FunctionTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import PrimaFunction
import Test exposing (..)


behavesLike : (a -> b) -> (a -> b) -> a -> Expectation
behavesLike f g x =
    Expect.equal (f x) (g x)


behavesLike2 : (a -> b -> c) -> (a -> b -> c) -> a -> b -> Expectation
behavesLike2 f g x y =
    Expect.equal (f x y) (g x y)


suite : Test
suite =
    describe "PrimaFunction tests"
        [ describe "(un)curry"
            [ fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "(uncurry >> curry) List.member == List.member" <|
                (List.member
                    |> (PrimaFunction.uncurry >> PrimaFunction.curry)
                    |> behavesLike2 List.member
                )
            ]
        , describe "flip"
            [ fuzz2 (Fuzz.list Fuzz.int) Fuzz.int "flip should flip it's arguments arguments" <|
                (List.member
                    |> PrimaFunction.flip
                    |> behavesLike2 (\x y -> List.member y x)
                )
            ]
        ]
