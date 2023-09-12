module RegexTest exposing (suite)

import Expect
import PrimaElmExtra.Regex exposing (email)
import Regex
import Test exposing (..)


suite : Test
suite =
    describe "Regex tests"
        [ describe "email"
            [ (\() ->
                "not an email"
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "Email validation should reject an arbitrary string"
            , (\() ->
                "valid@email.test"
                    |> Regex.contains email
                    |> Expect.equal True
              )
                |> test "Validation should succeed on a simple valid email"
            , (\() ->
                "longer.3M41L+WiTh_strange-CHARACTERS42@123but.5T1LL.V4L1D.indeed"
                    |> Regex.contains email
                    |> Expect.equal True
              )
                |> test "Validation should succeed on a more complex but still valid email"
            , (\() ->
                "trailing@dots.end."
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "No trailing dots allowed"
            , (\() ->
                "consecutive@dots..invalid"
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "No consecutive dots allowed"
            , (\() ->
                "excessive@long.domainextension"
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "Domain extension must be between 2 and 6 characters"
            , (\() ->
                "name@service.dom41n"
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "No numbers in domain extension"
            , (\() ->
                "name@service.domain!"
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "No symbols in domain extension"
            , (\() ->
                "String containing a valid@email.addr inside"
                    |> Regex.contains email
                    |> Expect.equal False
              )
                |> test "Regex must match the whole input"
            ]
        ]
