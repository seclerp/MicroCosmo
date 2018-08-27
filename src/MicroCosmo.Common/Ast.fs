module MicroCosmo.Ast
open System

type Program = Statement list

// Guid is unique component
and VariableDeclarationStatement = Identifier * TypeSpec * Expression option * bool * Guid

and TypeSpec =
    | NoneType
    | Any
    | String
    | Int
    | Double
    | Bool
    
and FunctionDeclarationStatement = Identifier * Parameters * TypeSpec * Statement * Guid
    
and Identifier = string
and Parameters = VariableDeclarationStatement list

// Guid is unique component
and IdentifierRef = { Identifier : string; Guid : Guid }

and Statement = 
    | FunctionDeclarationStatement of FunctionDeclarationStatement
    | VariableDeclarationStatement of VariableDeclarationStatement
    | ExpressionStatement of Expression
    | BlockStatement of BlockStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement
    
and BlockStatement = Statement list
and IfStatement = Expression * Statement * Statement option
and WhileStatement = Expression * Statement

and Expression = 
    | VariableAssignmentExpression of IdentifierRef * Expression * Guid
    | ArrayVariableAssignmentExpression of IdentifierRef * Expression * Expression * Guid
    | BinaryExpression of Expression * BinaryOperator * Expression * Guid
    | UnaryExpression of UnaryOperator * Expression * Guid
    | IdentifierExpression of IdentifierRef * Guid
    | ArrayIdentifierExpression of IdentifierRef * Expression * Guid
    | FunctionCallExpression of Identifier * Arguments * Guid
    | ArraySizeExpression of IdentifierRef * Guid
    | LiteralExpression of Literal * Guid
    | ArrayAllocationExpression of TypeSpec * Expression * Guid
    | Empty
    
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
    | StringLiteral of string
    | IntLiteral of int32
    | DoubleLiteral of double
    | BoolLiteral of bool