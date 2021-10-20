module StringTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import PrimaString
import Test exposing (..)


getFirstChar : String -> Maybe Char
getFirstChar =
    String.toList >> List.head


{-| Applies expectation when value is Just, passes otherwise
-}
whenJust : (a -> Expectation) -> Maybe a -> Expectation
whenJust f m =
    case m of
        Nothing ->
            Expect.pass

        Just x ->
            f x


suite : Test
suite =
    describe "Prima.Extra.String tests"
        [ describe "firstCharUppercase function"
            [ fuzz Fuzz.string "makes the first char uppercase (if present)" <|
                \str ->
                    str
                        |> PrimaString.capitalize
                        |> getFirstChar
                        |> whenJust
                            (\firstChar ->
                                if Char.isAlpha firstChar then
                                    Char.isUpper firstChar
                                        |> Expect.true "First char must be uppercase"

                                else
                                    Just firstChar
                                        |> Expect.equal (getFirstChar str)
                            )
            , fuzz Fuzz.string "preserves the remaining chars" <|
                \str ->
                    str
                        |> PrimaString.capitalize
                        |> String.dropLeft 1
                        |> Expect.equal (String.dropLeft 1 str)
            ]
        ]
