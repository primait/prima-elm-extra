module PrimaElmExtra.NotEmptyList exposing
    ( NotEmptyList
    , NotEmptyPartition(..)
    , all
    , any
    , append
    , appendList
    , concat
    , concatMap
    , cons
    , decoder
    , drop
    , dropToList
    , filter
    , filterMap
    , foldl
    , foldr
    , fromList
    , fromListSelectionSet
    , head
    , intersperse
    , length
    , listMap
    , map
    , map2
    , map3
    , map4
    , map5
    , maximum
    , member
    , minimum
    , notEmptyList
    , partition
    , product
    , reverse
    , singleton
    , sort
    , sortBy
    , sortWith
    , sum
    , tail
    , take
    , takeToList
    , toList
    , unzip
    )

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as JsonDecode


{-| This module defines a List that can never be empty. It should cover all the native List module API and some extra helpers.


# Type

@docs NotEmptyList


# Create

@docs singleton, repeat, range, notEmptyList, cons, fromList, decode


# Transform

@docs map, listMap, indexedMap, foldl, foldr, filter, filterMap


# Utilities

@docs length, reverse, member, all, any, maximum, minimum, sum, product


# Combine

@docs append, concat, concatMap, intersperse, map2, map3, map4, map5


# Sort

@docs sort, sortBy, sortWith


# Deconstruct

@docs isEmpty, head, tail, take, takeToList, drop, dropToList, partition, unzip, toList

-}
type NotEmptyList a
    = NotEmptyList
        { head : a
        , tail : List a
        }


{-| Creates a NotEmptyList from head element and tail

    notEmptyList 1 [ 2, 3 ] =
        NotEmptyList
            { head = 1
            , tail = [ 2, 3 ]
            }

-}
notEmptyList : a -> List a -> NotEmptyList a
notEmptyList first rest =
    NotEmptyList
        { head = first
        , tail = rest
        }


{-| Tries to create a NotEmptyList from a simple list
fromList [ 1, 2, 3 ] == Just { head = 1, tail = [ 2, 3 ] }
fromList [] == Nothing
-}
fromList : List a -> Maybe (NotEmptyList a)
fromList list =
    case list of
        x :: xs ->
            NotEmptyList { head = x, tail = xs } |> Just

        _ ->
            Nothing


{-| Create a NotEmptyList with only one element:

    singleton 1234 =
        { head = 1234, tail = [] }
    singleton "hi" =
        { head = "hi", tail = [] }

-}
singleton : a -> NotEmptyList a
singleton value =
    NotEmptyList { head = value, tail = [] }


{-| Typed NotEmptyList JSON Decoder. Breaks decoding if list is not empty
-}
decoder : JsonDecode.Decoder a -> JsonDecode.Decoder (NotEmptyList a)
decoder =
    JsonDecode.list
        >> JsonDecode.andThen
            (fromList
                >> Maybe.map JsonDecode.succeed
                >> Maybe.withDefault (JsonDecode.fail "List is empty")
            )


fromListSelectionSet : SelectionSet (List decodesTo) scope -> SelectionSet (NotEmptyList decodesTo) scope
fromListSelectionSet =
    SelectionSet.mapOrFail (fromList >> Result.fromMaybe "List can't be empty")


{-| Add an element to the front of a NotEmptyList.
cons 1 (notEmptyList 2 [ 3 ]) == { head = 1, tail = [ 2, 3 ] }
This operator is pronounced _cons_ for historical reasons, but you can think
of it like pushing an entry onto a stack.
-}
cons : a -> NotEmptyList a -> NotEmptyList a
cons a (NotEmptyList conf) =
    NotEmptyList { head = a, tail = conf.head :: conf.tail }



-- TRANSFORM


{-| Apply a function to every element of a NotEmptyList.
-}
map : (a -> b) -> NotEmptyList a -> NotEmptyList b
map mapper (NotEmptyList conf) =
    NotEmptyList
        { head = mapper conf.head
        , tail = List.map mapper conf.tail
        }


{-| Tries to apply a morphism on List onto a NotEmptyList.
-}
listMap : (List a -> List b) -> NotEmptyList a -> Maybe (NotEmptyList b)
listMap mapper =
    toList >> mapper >> fromList


{-| Reduce a NotEmptyList from the left.
-}
foldl : (a -> b -> b) -> b -> NotEmptyList a -> b
foldl func acc =
    toList >> List.foldl func acc


{-| Reduce a NotEmptyList from the right.
-}
foldr : (a -> b -> b) -> b -> NotEmptyList a -> b
foldr func acc =
    toList >> List.foldr func acc


{-| Keep elements that satisfy the test.
filter isEven (notEmptyList 1 [ 2, 3, 4, 5, 6 ]) == Just {head = 2, tail = [4, 6] }
filter isEven (notEmptyList 1 []) = Nothing
-}
filter : (a -> Bool) -> NotEmptyList a -> Maybe (NotEmptyList a)
filter isGood =
    toList
        >> List.filter isGood
        >> fromList


{-| Filter out certain values. For example, maybe you have a bunch of strings
from an untrusted source and you want to turn them into numbers:
-}
filterMap : (a -> Maybe b) -> NotEmptyList a -> Maybe (NotEmptyList b)
filterMap f =
    toList
        >> List.filterMap f
        >> fromList



-- UTILITIES


{-| Determine the length of a NotEmptyList (always GE than 1).
-}
length : NotEmptyList a -> Int
length =
    tail >> List.length >> (+) 1


{-| Reverse a NotEmptyList.
-}
reverse : NotEmptyList a -> NotEmptyList a
reverse nEL =
    case nEL |> toList |> List.reverse of
        x :: xs ->
            NotEmptyList { head = x, tail = xs }

        _ ->
            nEL


{-| Figure out whether a NotEmptyList contains a value.
-}
member : a -> NotEmptyList a -> Bool
member x xs =
    any (\a -> a == x) xs


{-| Determine if all elements satisfy some test.
-}
all : (a -> Bool) -> NotEmptyList a -> Bool
all isOkay =
    any (isOkay >> not)
        >> not


{-| Determine if any elements satisfy some test.
-}
any : (a -> Bool) -> NotEmptyList a -> Bool
any isOkay nEL =
    isOkay (head nEL) || List.any isOkay (tail nEL)


{-| Find the maximum element
-}
maximum : NotEmptyList comparable -> comparable
maximum nEL =
    List.foldl max (head nEL) (tail nEL)


{-| Find the minimum element
-}
minimum : NotEmptyList comparable -> comparable
minimum nEL =
    List.foldl min (head nEL) (tail nEL)


{-| Get the sum of the list elements.
-}
sum : NotEmptyList number -> number
sum numbers =
    foldl (+) 0 numbers


{-| Get the product of the list elements.
-}
product : NotEmptyList number -> number
product numbers =
    foldl (*) 1 numbers



-- COMBINE


{-| Combine two NotEmptyLists, combining them with the given function.
If one is longer, the extra elements are dropped.
totals : NotEmptyList Int -> NotEmptyList Int -> NotEmptyList Int
totals xs ys =
NotEmptyList.map2 (+) xs ys
-- totals [1,2,3][4,5,6] == [5,7,9]
pairs : NotEmptyList a -> NotEmptyList b -> NotEmptyList ( a, b )
pairs xs ys =
NotEmptyList.map2 Tuple.pair xs ys
-- pairs ["alice","bob","chuck"][2,5,7,8]
-- == [("alice",2),("bob",5),("chuck",7)]
-}
map2 : (a -> b -> result) -> NotEmptyList a -> NotEmptyList b -> NotEmptyList result
map2 mapper nELA nELB =
    NotEmptyList
        { head = mapper (head nELA) (head nELB)
        , tail = List.map2 mapper (tail nELA) (tail nELB)
        }


{-| -}
map3 : (a -> b -> c -> result) -> NotEmptyList a -> NotEmptyList b -> NotEmptyList c -> NotEmptyList result
map3 mapper nELA nELB nELC =
    NotEmptyList
        { head = mapper (head nELA) (head nELB) (head nELC)
        , tail = List.map3 mapper (tail nELA) (tail nELB) (tail nELC)
        }


{-| -}
map4 : (a -> b -> c -> d -> result) -> NotEmptyList a -> NotEmptyList b -> NotEmptyList c -> NotEmptyList d -> NotEmptyList result
map4 mapper nELA nELB nELC nELD =
    NotEmptyList
        { head = mapper (head nELA) (head nELB) (head nELC) (head nELD)
        , tail = List.map4 mapper (tail nELA) (tail nELB) (tail nELC) (tail nELD)
        }


{-| -}
map5 :
    (a -> b -> c -> d -> e -> result)
    -> NotEmptyList a
    -> NotEmptyList b
    -> NotEmptyList c
    -> NotEmptyList d
    -> NotEmptyList e
    -> NotEmptyList result
map5 mapper nELA nELB nELC nELD nELE =
    NotEmptyList
        { head = mapper (head nELA) (head nELB) (head nELC) (head nELD) (head nELE)
        , tail = List.map5 mapper (tail nELA) (tail nELB) (tail nELC) (tail nELD) (tail nELE)
        }


{-| Put two NotEmptyLists together (head is first list head).
NotEmptyList.append (notEmptyList 1 [1,2]) (notEmptyList 3 [5,8])
= NotEmptyList { head = 1, tail = [1,2,3,5,8]}
-}
append : NotEmptyList a -> NotEmptyList a -> NotEmptyList a
append nEL1 nEL2 =
    NotEmptyList
        { head = head nEL1
        , tail = List.append (tail nEL1) (toList nEL2)
        }


{-| Appends a List to a NotEmptyList.
-}
appendList : List a -> NotEmptyList a -> NotEmptyList a
appendList list nEL =
    NotEmptyList
        { head = head nEL
        , tail = List.append (tail nEL) list
        }


{-| Concatenate a bunch of NotEmptyList into a single NotEmptyList:
-}
concat : NotEmptyList (NotEmptyList a) -> NotEmptyList a
concat nEL =
    List.foldr append (head nEL) (tail nEL)


{-| Map a given function onto a NotEmptyList and flatten the resulting lists.
-}
concatMap : (a -> NotEmptyList b) -> NotEmptyList a -> NotEmptyList b
concatMap f list =
    concat (map f list)


{-| Places the given value between all members of the given NotEmptyList.
-}
intersperse : a -> NotEmptyList a -> NotEmptyList a
intersperse sep nEL =
    case tail nEL of
        [] ->
            NotEmptyList { head = head nEL, tail = [ sep ] }

        x :: [] ->
            NotEmptyList { head = head nEL, tail = [ sep, x ] }

        x :: xs ->
            NotEmptyList { head = head nEL, tail = sep :: List.intersperse sep (x :: xs) }



-- SORT


{-| Sort values from lowest to highest
sort [ 3, 1, 5 ] == [ 1, 3, 5 ]
-}
sort : NotEmptyList comparable -> NotEmptyList comparable
sort xs =
    sortBy identity xs


{-| Sort values by a derived property (still uses kernel's native sorter under the hood).
-}
sortBy : (a -> comparable) -> NotEmptyList a -> NotEmptyList a
sortBy sorter nEL =
    nEL
        |> toList
        |> List.sortBy sorter
        |> fromList
        |> Maybe.withDefault nEL


{-| Sort values with a custom comparison function (still uses kernel's native sorter under the hood).
-}
sortWith : (a -> a -> Order) -> NotEmptyList a -> NotEmptyList a
sortWith sorter nEL =
    nEL
        |> toList
        |> List.sortWith sorter
        |> fromList
        |> Maybe.withDefault nEL



-- DECONSTRUCT


{-| Extract the first element of a NotEmptyList.
-}
head : NotEmptyList a -> a
head (NotEmptyList config) =
    config.head


{-| Extract the rest of the NotEmptyList.
-}
tail : NotEmptyList a -> List a
tail (NotEmptyList config) =
    config.tail


{-| Take the first _n_ members of a NotEmptyList if result is not empty.
-}
take : Int -> NotEmptyList a -> Maybe (NotEmptyList a)
take n =
    toList >> List.take n >> fromList


{-| Take the first _n_ members of a NotEmptyList. Returns a normal emptiable list
-}
takeToList : Int -> NotEmptyList a -> List a
takeToList n =
    toList >> List.take n


{-| Drop the first _n_ members of a NotEmptyList if result is not empty.
-}
drop : Int -> NotEmptyList a -> Maybe (NotEmptyList a)
drop n =
    toList >> List.drop n >> fromList


{-| Drop the first _n_ members of a NotEmptyList. Returns a normal emptiable list
-}
dropToList : Int -> NotEmptyList a -> List a
dropToList n =
    toList >> List.drop n


{-| Decompose a NotEmptyList of tuples into a tuple of NotEmptyList.
-}
unzip : NotEmptyList ( a, b ) -> ( NotEmptyList a, NotEmptyList b )
unzip pairs =
    let
        ( headA, headB ) =
            ( pairs |> head |> Tuple.first, pairs |> head |> Tuple.second )

        ( tailA, tailB ) =
            List.unzip (tail pairs)
    in
    ( notEmptyList headA tailA, notEmptyList headB tailB )


{-| Converts a NotEmptyList into a simple List
-}
toList : NotEmptyList a -> List a
toList (NotEmptyList conf) =
    conf.head :: conf.tail


{-| Partition a not empty list based on some test. The first list contains all values
that satisfy the test, and the second list contains all the value that do not.

    partition (\x -> x < 3) [ 0, 1, 2, 3, 4, 5 ] == ( [ 0, 1, 2 ], [ 3, 4, 5 ] )

    partition isEven [ 0, 1, 2, 3, 4, 5 ] == ( [ 0, 2, 4 ], [ 1, 3, 5 ] )

-}
type NotEmptyPartition a
    = AllTruesPartition (NotEmptyList a)
    | AllFalsesPartition (NotEmptyList a)
    | BothPartition ( NotEmptyList a, NotEmptyList a )


partition : (a -> Bool) -> NotEmptyList a -> NotEmptyPartition a
partition pred nEL =
    let
        (NotEmptyList reversed) =
            reverse nEL
    in
    if pred reversed.head then
        List.foldl (partitionStep pred) (AllTruesPartition (singleton reversed.head)) reversed.tail

    else
        List.foldl (partitionStep pred) (AllFalsesPartition (singleton reversed.head)) reversed.tail


partitionStep : (a -> Bool) -> a -> NotEmptyPartition a -> NotEmptyPartition a
partitionStep pred x notEmptyPartition =
    case notEmptyPartition of
        AllTruesPartition trues ->
            if pred x then
                AllTruesPartition (cons x trues)

            else
                BothPartition ( trues, singleton x )

        AllFalsesPartition falses ->
            if pred x then
                BothPartition ( singleton x, falses )

            else
                AllFalsesPartition (cons x falses)

        BothPartition ( trues, falses ) ->
            if pred x then
                BothPartition ( cons x trues, falses )

            else
                BothPartition ( trues, cons x falses )
