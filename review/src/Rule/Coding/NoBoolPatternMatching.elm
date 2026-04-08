module Rule.Coding.NoBoolPatternMatching exposing (details, message, rule)

import Elm.Syntax.Expression as Expr exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Writer as Writer
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


message : String
message =
    "Expression uses pattern matching over a Bool value"


details : List String
details =
    [ "An if expression is more idiomatic when branching over the value of a boolean expression" ]


boolFromString : String -> Maybe Bool
boolFromString str =
    case str of
        "True" ->
            Just True

        "False" ->
            Just False

        _ ->
            Nothing


extractBoolPattern : Node Pattern -> Maybe Bool
extractBoolPattern nodePattern =
    case Node.value nodePattern of
        Pattern.NamedPattern { moduleName, name } [] ->
            case moduleName of
                [] ->
                    boolFromString name

                [ "Basics" ] ->
                    boolFromString name

                _ ->
                    Nothing

        _ ->
            Nothing


extractBools : Expr.Cases -> Maybe ( Node Expression, Node Expression )
extractBools cases =
    case cases of
        [ ( nodePattern1, nodeExpr1 ), ( Node _ Pattern.AllPattern, nodeExpr2 ) ] ->
            nodePattern1
                |> extractBoolPattern
                |> Maybe.map
                    (\boolPattern ->
                        if boolPattern then
                            ( nodeExpr1, nodeExpr2 )

                        else
                            ( nodeExpr2, nodeExpr1 )
                    )

        [ ( nodePattern1, nodeExpression1 ), ( nodePattern2, nodeExpression2 ) ] ->
            nodePattern2
                |> extractBoolPattern
                |> Maybe.map2 Tuple.pair (extractBoolPattern nodePattern1)
                |> Maybe.andThen
                    (\pair ->
                        case pair of
                            ( True, False ) ->
                                Just ( nodeExpression1, nodeExpression2 )

                            ( False, True ) ->
                                Just ( nodeExpression2, nodeExpression1 )

                            _ ->
                                Nothing
                    )

        _ ->
            Nothing


makeFix range expression whenTrue whenFalse =
    Rule.errorWithFix { message = message, details = details }
        range
        [ Expr.IfBlock expression whenTrue whenFalse
            |> Node range
            |> Writer.writeExpression
            |> Writer.write
            |> Fix.replaceRangeBy range
        ]


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor (Node range value) =
    case value of
        Expr.CaseExpression { expression, cases } ->
            cases
                |> extractBools
                |> Maybe.map (\( whenTrue, whenFalse ) -> [ makeFix range expression whenTrue whenFalse ])
                |> Maybe.withDefault []

        _ ->
            []


{-| Forbids the usage of a `case` expression instead of an `if` expression and provides a fix.


### Fail

    case b of
        True ->
            x

        False ->
            y


### Success

    if b then
        x

    else
        y


#### Details

The rule also handles catchall pattern

    case b of
        True ->
            x

        _ ->
            y

But doesn't handle (by design) nested booleans

    view =
        case ( userType, loggedIn ) of
            ( Admin, _ ) ->
                viewDashboard

            ( User, True ) ->
                viewDashboard

            _ ->
                viewNotFound

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Coding.NoBoolPatternMatching" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
