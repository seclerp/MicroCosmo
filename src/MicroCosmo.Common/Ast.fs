module Ast

type Program = Declaration list

and Declaration = 
    | LocalVariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration

and VariableDeclaration =
    | ScalarVariableDeclaration of TypeSpec * Identifier
    | ArrayVariableDeclaration of TypeSpec * Identifier
    
and TypeSpec =
    | None
    | Any
    | String
    | Int
    | Double
    | Bool
    
and FunctionDeclaration = 
    | TypedFunctionDeclaration of Identifier * Parameters * TypeSpec * BlockStatement
    | NodeFunctionDeclaration of Identifier * Parameters * BlockStatement
    
and Identifier = string
and Parameters = VariableDeclaration list
and IdentifierRef = { Identifier : string }

and Statement = 
    | ExpressionStatement of ExpressionStatement
    | BlockStatement of BlockStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement
    
and ExpressionStatement = 
    | Expression of Expression
    | Nop
    
and BlockStatement = VariablesScope * Statement list
and VariablesScope = VariableDeclaration list
and IfStatement = Expression * Statement * Statement option
and WhileStatement = Expression * Statement

and Expression = 
    | VariableAssignmentExpression of IdentifierRef * Expression
    | ArrayVariableAssignmentExpression of IdentifierRef * Expression * Expression
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | IdentifierExpression of IdentifierRef
    | ArrayIdentifierExpression of IdentifierRef * Expression
    | FunctionCallExpression of Identifier * Arguments
    | ArraySizeExpression of IdentifierRef
    | LiteralExpression of Literal
    | ArrayAllocationExpression of TypeSpec * Expression
    
and BinaryOperator =
    | Eq
    | NotEq
    | Sum 
    | Diff
    | Mult
    | Div
    | Mod
    | Pow
    | SumAssign
    | DiffAssign
    | MultAssign
    | DivAssign
    | ModAssign
    | PowAssign
    | Gt
    | Lt
    | GtEq
    | LtEq
    | Or
    | And
    
and UnaryOperator =
    | Not 
    | Plus
    | Minus
    | PostPlusPLus
    | PostMinusMinus
    | PrePlusPLus
    | PreMinusMinus

and Arguments = Expression list

and Literal =
    | String of string
    | Int of int32
    | Double of double
    | Bool of bool