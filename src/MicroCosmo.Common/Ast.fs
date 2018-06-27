module MicroCosmo.Ast

type Program = Declaration list

and Declaration = 
    | VariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration

and VariableDeclaration = Identifier * TypeSpec * Expression * bool

and TypeSpec =
    | NoneType
    | Any
    | String
    | Int
    | Double
    | Bool
    
and FunctionDeclaration = Identifier * Parameters * TypeSpec * Statement
    
and Identifier = string
and Parameters = VariableDeclaration list
and IdentifierRef = { Identifier : string }

and Statement = 
    | ExpressionStatement of Expression
    | BlockStatement of BlockStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement
    
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