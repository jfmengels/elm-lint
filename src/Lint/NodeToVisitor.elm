module Lint.NodeToVisitor exposing (declarationsIntoVisitors, expressionToVisitors)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node, value)
import Lint.Rule exposing (Direction(..), Visitor, evaluateExpression, finalEvaluation)


createExitAndEnterWithChildren : (Direction -> nodeType -> Visitor context) -> nodeType -> List (Visitor context) -> List (Visitor context)
createExitAndEnterWithChildren toVisitor node children =
    List.concat
        [ [ toVisitor Enter node ]
        , children
        , [ toVisitor Exit node ]
        ]


moduleVisitor : Visitor context
moduleVisitor rule context =
    finalEvaluation rule context


expressionVisitor : Direction -> Node Expression -> Visitor context
expressionVisitor direction node rule context =
    evaluateExpression rule context direction node


functionToExpression : Function -> Node Expression
functionToExpression { documentation, signature, declaration } =
    let
        { name, arguments, expression } =
            value declaration
    in
    expression


expressionToVisitors : Node Expression -> List (Visitor context)
expressionToVisitors node =
    let
        children : List (Node Expression)
        children =
            case value node of
                Application expressions ->
                    expressions

                Literal _ ->
                    []

                Integer _ ->
                    []

                Floatable _ ->
                    []

                UnitExpr ->
                    []

                ListExpr elements ->
                    elements

                FunctionOrValue _ _ ->
                    []

                RecordExpr fields ->
                    List.map (value >> (\( name, expr ) -> expr)) fields

                RecordUpdateExpression name setters ->
                    List.map (value >> (\( field, expr ) -> expr)) setters

                ParenthesizedExpression expr ->
                    [ expr ]

                OperatorApplication operator direction left right ->
                    case direction of
                        Left ->
                            [ left, right ]

                        Right ->
                            [ right, left ]

                        Non ->
                            [ left, right ]

                IfBlock cond then_ else_ ->
                    [ cond, then_, else_ ]

                LetExpression { expression, declarations } ->
                    List.map
                        (\declaration ->
                            case value declaration of
                                LetFunction function ->
                                    functionToExpression function

                                LetDestructuring pattern expr ->
                                    expr
                        )
                        declarations
                        ++ [ expression ]

                CaseExpression { expression, cases } ->
                    [ expression ]
                        ++ List.map (\( pattern, caseExpression ) -> caseExpression) cases

                LambdaExpression { args, expression } ->
                    [ expression ]

                TupledExpression expressions ->
                    expressions

                -- TODO Implement the rest
                _ ->
                    []

        childrenVisitors =
            List.concatMap expressionToVisitors children
    in
    createExitAndEnterWithChildren expressionVisitor node childrenVisitors


declarationToVisitors : Declaration -> List (Visitor context)
declarationToVisitors declaration =
    let
        childrenVisitors =
            case declaration of
                FunctionDeclaration function ->
                    functionToExpression function |> expressionToVisitors

                -- TODO Implement the rest
                _ ->
                    []
    in
    -- createExitAndEnterWithChildren statementVisitor declaration childrenVisitors
    childrenVisitors


declarationsIntoVisitors : List (Node Declaration) -> List (Visitor context)
declarationsIntoVisitors declarations =
    declarations
        |> List.concatMap (value >> declarationToVisitors)
        |> (\allVisitors -> List.append allVisitors [ moduleVisitor ])