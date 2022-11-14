module Render exposing
    ( htmlIf, listIf, listUnless, htmlUnless
    , listOrNothing, maybe, maybeMap2, orNothing
    , maybeMap
    )

{-| Collection of rendering utilities


# Bool based conditionals

@docs htmlIf, listIf, listUnless, htmlUnless


# Maybe based conditionals

@docs listOrNothing, maybe, maybeMap2, orNothing

-}

import Html exposing (Html, text)


{-| Old render-if. Prints an html node when true
-}
htmlIf : Bool -> Html a -> Html a
htmlIf check html =
    if check then
        html

    else
        text ""


{-| Prints a list of html nodes when true
-}
listIf : Bool -> List (Html a) -> List (Html a)
listIf check html =
    if check then
        html

    else
        []


{-| Prints a list of html nodes when false
-}
listUnless : Bool -> List (Html a) -> List (Html a)
listUnless check html =
    listIf (not check) html


{-| Prints a Maybe list of html nodes when exists
-}
listOrNothing : Maybe (List (Html a)) -> List (Html a)
listOrNothing maybeHtml =
    Maybe.withDefault [] maybeHtml


{-| Prints an html node when false
-}
htmlUnless : Bool -> Html a -> Html a
htmlUnless check =
    htmlIf (not check)


{-| Prints an html node when is Just
-}
maybe : Maybe a -> Html msg -> Html msg
maybe theMaybe html =
    case theMaybe of
        Just _ ->
            html

        Nothing ->
            text ""


{-| Prints an html node through a render function when the value is Just, passing the value as the argument
-}
maybeMap : (a -> Html msg) -> Maybe a -> Html msg
maybeMap renderer m =
    case m of
        Just a ->
            renderer a

        Nothing ->
            text ""


{-| Prints an html node through a render function when the values are all Just, passing the values as the argument
-}
maybeMap2 : (a -> b -> Html msg) -> Maybe a -> Maybe b -> Html msg
maybeMap2 renderer maybeA maybeB =
    case ( maybeA, maybeB ) of
        ( Just a, Just b ) ->
            renderer a b

        _ ->
            text ""


{-| Resolves a Maybe (Html msg)
-}
orNothing : Maybe (Html msg) -> Html msg
orNothing maybeHtml =
    Maybe.withDefault (text "") maybeHtml
