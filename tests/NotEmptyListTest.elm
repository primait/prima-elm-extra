module NotEmptyListTest exposing (suite)

import Expect
import PrimaElmExtra.NotEmptyList as NotEmptyList
import Test exposing (..)


suite : Test
suite =
    describe "NotEmptyList tests"
        [ (\() ->
            NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]
                |> NotEmptyList.toList
                |> Expect.equal [ 1, 2, 3, 4, 5 ]
          )
            |> test "toList mantains order"
        , describe "partition"
            [ (\() ->
                NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]
                    |> NotEmptyList.partition ((>) 4)
                    |> Expect.equal (NotEmptyList.BothPartition ( NotEmptyList.notEmptyList 1 [ 2, 3 ], NotEmptyList.notEmptyList 4 [ 5 ] ))
              )
                |> test "Not empty list produces both not empty partition"
            , (\() ->
                NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]
                    |> NotEmptyList.partition ((>=) 4)
                    |> Expect.equal (NotEmptyList.BothPartition ( NotEmptyList.notEmptyList 1 [ 2, 3, 4 ], NotEmptyList.notEmptyList 5 [] ))
              )
                |> test "Not empty list produces both not empty partition (nle variant)"
            , (\() ->
                NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]
                    |> NotEmptyList.partition ((>) 10)
                    |> Expect.equal (NotEmptyList.AllTruesPartition (NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]))
              )
                |> test "Not empty list produces not empty trues partition"
            , (\() ->
                NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]
                    |> NotEmptyList.partition ((>) 0)
                    |> Expect.equal (NotEmptyList.AllFalsesPartition (NotEmptyList.notEmptyList 1 [ 2, 3, 4, 5 ]))
              )
                |> test "Not empty list produces not empty falses partition"
            ]
        ]
