module MicroCosmo.Ast

type Program = Statement list

and VariableDeclarationStatement = Identifier * TypeSpec * Expression option * bool

and TypeSpec =
    | NoneType
    | Any
    | String
    | Int
    | Double
    | Bool
    
and FunctionDeclarationStatement = Identifier * Parameters * TypeSpec * Statement
    
and Identifier = string
and Parameters = VariableDeclarationStatement list
and IdentifierRef = { Identifier : string }

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