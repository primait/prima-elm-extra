module TimeTest exposing (..)

import Expect
import Fuzz
import Prima.Extra.Time
import Test exposing (..)


suite : Test
suite =
    describe "Prima.Extra.Float tests"
        [ let
            isInputValid n =
                n >= 1 && n <= 12
          in
          describe "intToMaybeMonth"
            [ fuzz Fuzz.int "Discards invalid inputs" <|
                \n ->
                    if isInputValid n then
                        Expect.pass

                    else
                        n
                            |> Prima.Extra.Time.intToMaybeMonth
                            |> Expect.equal Nothing
            , fuzz2 Fuzz.int Fuzz.int "Is injective (within its domain)" <|
                \n1 n2 ->
                    if not (isInputValid n1 && isInputValid n2) || n1 == n2 then
                        Expect.pass

                    else
                        Expect.notEqual
                            (Prima.Extra.Time.intToMaybeMonth n1)
                            (Prima.Extra.Time.intToMaybeMonth n2)
            ]
        ]
