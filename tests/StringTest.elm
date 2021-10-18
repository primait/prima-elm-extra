module StringTest exposing (..)

import Expect
import Fuzz
import Prima.Extra.String
import Test exposing (..)


getFirstChar : String -> Maybe Char
getFirstChar =
    String.toList >> List.head


suite : Test
suite =
    describe "Prima.Extra.String tests"
        [ describe "isEmpty function"
            [ test "Works on empty strings" <|
                \_ -> Prima.Extra.String.isEmpty "" |> Expect.equal True
            , test "Works on nonempty strings" <|
                \_ -> Prima.Extra.String.isEmpty "not empty" |> Expect.equal False
            ]
        , describe "firstCharUppercase function"
            [ fuzz Fuzz.string "makes the first char uppercase (if present)" <|
                \str ->
                    case
                        str
                            |> Prima.Extra.String.capitalize
                            |> getFirstChar
                    of
                        Nothing ->
                            Expect.pass

                        Just firstChar ->
                            if Char.isAlpha firstChar then
                                Char.isUpper firstChar
                                    |> Expect.true "First char must be uppercase"

                            else
                                Just firstChar
                                    |> Expect.equal (getFirstChar str)
            , fuzz Fuzz.string "preserves the remaining chars" <|
                \str ->
                    str
                        |> Prima.Extra.String.capitalize
                        |> String.dropLeft 1
                        |> Expect.equal (String.dropLeft 1 str)
            ]
        ]
