module Rule.Coding.NoPipeRecordGetter exposing (details, message, rule)

import Elm.Syntax.Expression as Expr exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Writer as Writer
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


message : String
message =
    "Invalid usage of record field getter"


details : List String
details =
    [ "The regular record access is preferred for consistency" ]


{-| fix record |> .getterName
in range nodeRange
-}
fix : { record : Node Expression, getterName : String, nodeRange : Range } -> Error {}
fix { record, getterName, nodeRange } =
    Rule.errorWithFix { message = message, details = details }
        nodeRange
        [ getterName
            |> Node Range.emptyRange
            |> Expr.RecordAccess record
            |> Node Range.emptyRange
            |> Writer.writeExpression
            |> Writer.write
            |> Fix.replaceRangeBy nodeRange
        ]


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expr.OperatorApplication "|>" Infix.Left record f ->
            case ( Node.value record, Node.value f ) of
                ( Expr.FunctionOrValue _ _, Expr.RecordAccessFunction name ) ->
                    [ fix
                        { nodeRange = Node.range node
                        , getterName = String.dropLeft 1 name
                        , record = record
                        }
                    ]

                _ ->
                    []

        _ ->
            []


{-| Forbids the usage of the record field getter syntax (.field) when used as the first step of a pipe


## Fail

       model
        |> .users
        |> f


## Success

       model.users
       |> f

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Coding.NoPipeRecordGetterRule" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
