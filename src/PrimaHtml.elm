module PrimaHtml exposing
    ( maybeMap2Render
    , maybeMapRender
    , renderIf
    , renderListIf
    , renderListOrNothing
    , renderListUnless
    , renderMaybe
    , renderOrNothing
    , renderUnless
    )

import Html exposing (Html, text)


renderIf : Bool -> Html a -> Html a
renderIf check html =
    if check then
        html

    else
        text ""


renderListIf : Bool -> List (Html a) -> List (Html a)
renderListIf check html =
    if check then
        html

    else
        []


renderListUnless : Bool -> List (Html a) -> List (Html a)
renderListUnless check html =
    renderListIf (not check) html


renderListOrNothing : Maybe (List (Html a)) -> List (Html a)
renderListOrNothing maybeHtml =
    Maybe.withDefault [] maybeHtml


renderUnless : Bool -> Html a -> Html a
renderUnless check =
    renderIf (not check)


renderMaybe : Maybe a -> Html msg -> Html msg
renderMaybe theMaybe html =
    case theMaybe of
        Just _ ->
            html

        Nothing ->
            text ""


maybeMapRender : (a -> Html msg) -> Maybe a -> Html msg
maybeMapRender renderer maybe =
    case maybe of
        Just a ->
            renderer a

        Nothing ->
            text ""


maybeMap2Render : (a -> b -> Html msg) -> Maybe a -> Maybe b -> Html msg
maybeMap2Render renderer maybeA maybeB =
    case ( maybeA, maybeB ) of
        ( Just a, Just b ) ->
            renderer a b

        _ ->
            text ""


renderOrNothing : Maybe (Html a) -> Html a
renderOrNothing maybeHtml =
    Maybe.withDefault (text "") maybeHtml
