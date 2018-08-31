module MicroCosmo.SyntaxAnalysis.Parser

open FParsec
open MicroCosmo.Ast
open MicroCosmo.Terminants
open MicroCosmo.SyntaxAnalysis.ParserHelpers

open System
open MicroCosmo

let getGuid = Guid.NewGuid

let typeSpec : Parser<Ast.TypeSpec, unit> = 
    choice_ws [
        attempt (keyword NONE    |>> (fun _ -> Ast.NoneType)) ;
        attempt (keyword STRING  |>> (fun _ -> Ast.String)) ;
        attempt (keyword INT     |>> (fun _ -> Ast.Int)) ;
        attempt (keyword DOUBLE  |>> (fun _ -> Ast.Double)) ;
                (keyword BOOL    |>> (fun _ -> Ast.Bool)) ;
    ]

let identifier : Parser<Ast.Identifier, unit> = 
    regex_ws IDENTIFIER |>> (fun a -> string a)

/// Literals

let stringLiteral : Parser<Ast.Literal, unit> = 
    literal STRING_LIT                     
        |>> (fun s -> Ast.StringLiteral((s.Substring(1, s.Length - 2))))

let boolLiteral : Parser<Ast.Literal, unit> = 
    let trueLiteral = str_ws TRUE_LIT      
                        |>> (fun _ -> Ast.BoolLiteral true)
                        
    let falseLiteral = str_ws FALSE_LIT    
                        |>> (fun _ -> Ast.BoolLiteral false)
                        
    trueLiteral <|> falseLiteral
    
let intLiteral : Parser<Ast.Literal, unit>  =
    literal INT_LIT 
        |>> (fun a -> Ast.IntLiteral (int a))

let doubleLiteral : Parser<Ast.Literal, unit> =
    literal DOUBLE_LIT 
        |>> (fun a -> Ast.DoubleLiteral (double a))

let literal : Parser<Ast.Literal, unit> = 
    choice_ws [
        attempt stringLiteral ;
        attempt boolLiteral ;
        attempt doubleLiteral ;
                intLiteral ;
    ]



/// Expressions

let expression, expressionImpl = createParserForwardedToRef()

let literalExpression : Parser<Ast.Expression, unit> = 
    literal 
        |>> (fun a -> Ast.LiteralExpression (a, getGuid()))

let assignmentExpression : Parser<Ast.Expression, unit> = 
    (pipe2 (identifier) (symbol EQ >>. expression)
        (fun a b -> Ast.VariableAssignmentExpression ({ Identifier = a; Guid = getGuid() }, b, getGuid()))) ;
    
let arguments : Parser<Ast.Arguments, unit> = sepBy_ws expression (symbol COMMA)
    
let identifierExpression : Parser<Ast.Expression, unit> =
    choice_ws [
        attempt (pipe2 (identifier) (symbol OPENPAREN >>. arguments .>> symbol CLOSEPAREN)
            (fun a b -> Ast.FunctionCallExpression (a, b, getGuid()))) ;
        identifier |>> (fun x -> Ast.IdentifierExpression ({ Identifier = x; Guid = getGuid(); }, getGuid())) ;
    ]

/// Operators    

let opp = new OperatorPrecedenceParser<Ast.Expression, unit, unit>()
let termsExpression = opp.ExpressionParser

let termParser = 
    choice_ws [ 
        attempt assignmentExpression ; 
        attempt literalExpression ; 
        attempt identifierExpression ; 
        between (symbol OPENPAREN) (symbol CLOSEPAREN) termsExpression 
    ]
    
let identifierFromExpression = function
    | Ast.IdentifierExpression (i, _) -> {i with Guid = getGuid()}
    | _ -> raise (CompilerErrors.syntaxError (sprintf "Identifier expected"))
    
opp.TermParser <- termParser

opp.AddOperator(InfixOperator(OR, ws, 1, Associativity.Left, fun x y ->     (binary x Ast.Eq y (getGuid()))))
opp.AddOperator(InfixOperator(IS, ws, 2, Associativity.Left, fun x y ->     (binary x Ast.Eq y (getGuid()))))

opp.AddOperator(InfixOperator(TO, ws, 2, Associativity.Left, fun x y ->     (binary x Ast.To y (getGuid()))))

opp.AddOperator(InfixOperator(LTEQ, ws, 2, Associativity.Left, fun x y ->   (binary x Ast.LtEq y (getGuid()))))
opp.AddOperator(InfixOperator(LT, ws, 2, Associativity.Left, fun x y ->     (binary x Ast.Lt y (getGuid()))))

opp.AddOperator(InfixOperator(GTEQ, ws, 2, Associativity.Left, fun x y ->   (binary x Ast.GtEq y (getGuid()))))
opp.AddOperator(InfixOperator(GT, ws, 2, Associativity.Left, fun x y ->     (binary x Ast.Gt y (getGuid()))))

opp.AddOperator(InfixOperator(AND, ws, 3, Associativity.Left, fun x y ->    (binary x Ast.And y (getGuid()))))

opp.AddOperator(InfixOperator(PLUS, ws, 1, Associativity.Left, fun x y ->            (binary x Ast.Sum y (getGuid()))))
opp.AddOperator(InfixOperator(MINUS, ws, 1, Associativity.Left, fun x y ->           (binary x Ast.Diff y (getGuid()))))
opp.AddOperator(InfixOperator(ASTERISK, ws, 2, Associativity.Left, fun x y ->        (binary x Ast.Mult y (getGuid()))))
opp.AddOperator(InfixOperator(FORWARDSLASH, ws, 2, Associativity.Left, fun x y ->    (binary x Ast.Div y (getGuid()))))
opp.AddOperator(InfixOperator(DOUBLEASTERISK, ws, 3, Associativity.Right, fun x y -> (binary x Ast.Pow y (getGuid()))))

opp.AddOperator(PrefixOperator(NOT, ws, 4, true, fun x -> (unary x Ast.Not (getGuid()))))
opp.AddOperator(PrefixOperator(MINUS, ws, 4, true, fun x -> (unary x Ast.Minus (getGuid()))))
opp.AddOperator(PrefixOperator(PLUS, ws, 4, true, fun x -> (unary x Ast.Plus (getGuid()))))

opp.AddOperator(PrefixOperator(PLUSPLUS, ws, 5, true, fun x -> (Ast.VariableAssignmentExpression((identifierFromExpression x), (binary (x) (Ast.Sum) (Ast.LiteralExpression(Ast.IntLiteral(1), getGuid())) (getGuid())), (getGuid())))))
opp.AddOperator(PrefixOperator(MINUSMINUS, ws, 5, true, fun x -> (Ast.VariableAssignmentExpression((identifierFromExpression x), (binary (x) (Ast.Diff) (Ast.LiteralExpression(Ast.IntLiteral(1), getGuid())) (getGuid())), (getGuid())))))
    
opp.AddOperator(PostfixOperator(PLUSPLUS, ws, 5, true, fun x -> (Ast.VariableAssignmentExpression((identifierFromExpression x), (binary (x) (Ast.Sum) (Ast.LiteralExpression(Ast.IntLiteral(1), getGuid())) (getGuid())), (getGuid())))))
opp.AddOperator(PostfixOperator(MINUSMINUS, ws, 5, true, fun x -> (Ast.VariableAssignmentExpression((identifierFromExpression x), (binary (x) (Ast.Diff) (Ast.LiteralExpression(Ast.IntLiteral(1), getGuid())) (getGuid())), (getGuid())))))

do expressionImpl := 
    choice_ws [
        attempt assignmentExpression ;
        attempt termsExpression ;
        attempt literalExpression ;
        identifierExpression ;
    ]
    
    
    
/// Statements

let statement, statementImpl = createParserForwardedToRef()

let statements : Parser<Ast.Statement list, unit> = many_ws statement

let breakStatement : Parser<Ast.Statement, unit> = 
    (keyword BREAK |>> (fun _ -> Ast.BreakStatement))

let returnStatement : Parser<Ast.Statement, unit> = 
    choice_ws [
        attempt (keyword RETURN >>. expression |>> (fun a -> Ast.ReturnStatement (Some a))) ;
                (keyword RETURN |>> (fun _ -> Ast.ReturnStatement None)) ;
    ]

let blockStatement =
    symbol OPENCURLY >>. statements .>> symbol CLOSECURLY |>>
        (fun a -> Ast.BlockStatement (a))
    
let ifStatement : Parser<Ast.Statement, unit> =
    choice_ws [
        attempt (pipe3 (keyword IF >>. symbol OPENPAREN >>. expression .>> symbol CLOSEPAREN) (statement) (keyword ELSE >>. statement)
            (fun a b c -> Ast.IfStatement (a, b, Some c))) ;
        (pipe2 (keyword IF >>. symbol OPENPAREN >>. expression .>> symbol CLOSEPAREN) (statement) 
            (fun a b -> Ast.IfStatement (a, b, None)));
    ]
        
let whileStatement : Parser<Ast.Statement, unit> =
    pipe2 (keyword WHILE >>. symbol OPENPAREN >>. expression .>> symbol CLOSEPAREN) (statement) 
        (fun a b -> Ast.WhileStatement (a, b))

let expressionStatement : Parser<Ast.Statement, unit> =
    expression |>> (fun a -> Ast.ExpressionStatement (a))
    
    
let parameterStatement : Parser<Ast.VariableDeclarationStatement, unit> =
        (pipe2 (identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> (a, b, None, getGuid()))) ;

let parametersStatement : Parser<Ast.Parameters, unit> = sepBy_ws parameterStatement (symbol COMMA)

let functionDeclarationStatement : Parser<Ast.Statement, unit> = 
    choice_ws [
        attempt (pipe4 (keyword FUNC >>. identifier) (symbol OPENPAREN >>. parametersStatement .>> symbol CLOSEPAREN)
            (symbol COLON >>. typeSpec <!> "asasas") (blockStatement)
            (fun a b c d -> Ast.FunctionDeclarationStatement (a, b, c, d, getGuid()))) ;
        (pipe3 (keyword FUNC >>. identifier) (symbol OPENPAREN >>. parametersStatement .>> symbol CLOSEPAREN)
            (blockStatement)
            (fun a b c -> Ast.FunctionDeclarationStatement (a, b, Ast.NoneType, c, getGuid()))) ;
    ]

let variableDeclarationStatement : Parser<Ast.Statement, unit> = 
    choice_ws [
        attempt (pipe3 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) (symbol EQ >>. expression)
            (fun a b c -> Ast.VariableDeclarationStatement (a, b, Some c, getGuid()))) ;
        (pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> Ast.VariableDeclarationStatement (a, b, None, getGuid()))) ;
    ]
    
/// Comments

let singleLineComment : Parser<Ast.Statement, unit> = 
    (pchar COMMENT_START >>. restOfLine true) |>> (fun x -> Ast.CommentStatement x)

do statementImpl := 
    choice_ws [
        attempt singleLineComment ;
        attempt functionDeclarationStatement ;
        attempt variableDeclarationStatement ;
        attempt ifStatement ;
        attempt whileStatement;
        attempt returnStatement ;
        attempt breakStatement ;
        attempt blockStatement ;
        expressionStatement ;
    ]

/// Declarations

let declarationStatement = 
    choice_ws [
        variableDeclarationStatement ;
        functionDeclarationStatement ;
        singleLineComment ;
    ]
    
let declarationStatementList = many_ws declarationStatement

let program = declarationStatementList .>> eof

let parse (input : string) =
    match run program input with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error (CompilerErrors.syntaxError errorMsg)
