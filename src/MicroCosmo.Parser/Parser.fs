module MicroCosmo.Parser

open FParsec
open MicroCosmo.Terminants
open MicroCosmo.ParserHelpers

// Non terminal productions
#nowarn "40"

let expression, expressionImpl = createParserForwardedToRef()
let statement, statementImpl = createParserForwardedToRef()

let rec typeSpec : Parser<Ast.TypeSpec, unit> = 
    choice_ws [
        attempt (keyword NONE    |>> (fun _ -> Ast.None)) ;
        attempt (keyword ANY     |>> (fun _ -> Ast.Any)) ;
        attempt (keyword STRING  |>> (fun _ -> Ast.String)) ;
        attempt (keyword INT     |>> (fun _ -> Ast.Int)) ;
        attempt (keyword DOUBLE  |>> (fun _ -> Ast.Double)) ;
                (keyword BOOL    |>> (fun _ -> Ast.Bool)) ;
    ]

let identifier : Parser<Ast.Identifier, unit> = 
    regex_ws IDENTIFIER |>> (fun a -> string a)

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

let arrayAllocation : Parser<Ast.Expression, unit>  =
    let typeSpec_ws = typeSpec .>> ws
    let array_ws = regex_ws ARRAY .>> ws
    (typeSpec_ws .>> array_ws) .>>. expression 
        |>> (fun (a, b) -> Ast.ArrayAllocationExpression (a, b))

let literal : Parser<Ast.Literal, unit> = 
    choice_ws [
        attempt stringLiteral ;
        attempt boolLiteral ;
        attempt doubleLiteral ;
        intLiteral ;
    ]
    
let literalExpression : Parser<Ast.Expression, unit> = 
    literal 
        |>> (fun a -> Ast.LiteralExpression a)


let assignmentExpression : Parser<Ast.Expression, unit> = 
    choice_ws [
        attempt (pipe3 (identifier) (symbol OPENSQUARE >>. expression .>> symbol CLOSESQUARE) (symbol EQ >>. expression) 
            (fun a b c -> Ast.ArrayVariableAssignmentExpression ({ Identifier = a }, b, c)));
        (pipe2 (identifier) (symbol EQ >>. expression)
            (fun a b -> Ast.VariableAssignmentExpression ({ Identifier = a }, b))) ;
    ]
    

// TODO: 
do expressionImpl := 
    literalExpression


let breakStatement : Parser<Ast.Statement, unit> = 
    (keyword BREAK |>> (fun _ -> Ast.BreakStatement))

let returnStatement : Parser<Ast.Statement, unit> = 
    choice_ws [
        attempt (keyword RETURN >>. expression |>> (fun a -> Ast.ReturnStatement (Some a)))
        (keyword RETURN |>> (fun _ -> Ast.ReturnStatement None))
    ]

let ifStatement : Parser<Ast.Statement, unit> =
    choice_ws [
        attempt (pipe3 (keyword IF >>. expression) (statement) (keyword ELSE >>. statement)
            (fun a b c -> Ast.IfStatement (a, b, Some c))) ;
        (pipe2 (keyword IF >>. expression) (statement) 
            (fun a b -> Ast.IfStatement (a, b, None)));
    ]

let localDeclaration : Parser<Ast.VariableDeclaration, unit> = 
    choice_ws [
        attempt (pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> (a, b, Ast.Empty, true))) ;
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> (a, b, Ast.Empty, false)) ;
    ]

let localDeclarations : Parser<Ast.VariableDeclaration list, unit> = 
    many_ws localDeclaration

let statements : Parser<Ast.Statement list, unit> = many_ws statement

let blockStatement : Parser<Ast.Statement, unit> =
    pipe2 (symbol OPENCURLY >>. localDeclarations) (statements .>> symbol CLOSECURLY) 
        (fun a b -> Ast.BlockStatement (a, b))
        
let whileStatement : Parser<Ast.Statement, unit> =
    pipe2 (keyword WHILE >>. expression) (symbol OPENCURLY >>. statement .>> symbol CLOSECURLY) 
        (fun a b -> Ast.WhileStatement (a, b))

let expressionStatement : Parser<Ast.Statement, unit> =
    expression |>> (fun a -> Ast.ExpressionStatement (a))
    
do statementImpl := 
    choice_ws [
        attempt expressionStatement ;
        attempt blockStatement ;
        attempt ifStatement ;
        attempt whileStatement ;
        attempt returnStatement ;
        breakStatement ;
    ]

let parameter : Parser<Ast.VariableDeclaration, unit> =
    choice_ws [
        attempt (pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> (a, b, Ast.Empty, true))) ;
        (pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> (a, b, Ast.Empty, false))) ;
    ]

let parameters : Parser<Ast.Parameters, unit> = sepBy_ws parameter (symbol COMMA)

let functionDeclaration : Parser<Ast.Declaration, unit> = 
    choice_ws [
        attempt (pipe4 (keyword FUNC >>. identifier) (symbol OPENPAREN >>. parameters .>> symbol CLOSEPAREN)
            (symbol COLON >>. typeSpec) (blockStatement)
            (fun a b c d -> Ast.FunctionDeclaration (a, b, c, d))) ;
        (pipe3 (keyword FUNC >>. identifier) (symbol OPENPAREN >>. parameters .>> symbol CLOSEPAREN)
            (blockStatement)
            (fun a b c -> Ast.FunctionDeclaration (a, b, Ast.None, c))) ;
    ]

let parametersDeclaration : Parser<Ast.Declaration, unit> =
    choice_ws [
        attempt (pipe3 identifier (symbol COLON >>. typeSpec .>> keyword ARRAY) (symbol EQ >>. expression)
            (fun a b c -> Ast.VariableDeclaration (a, b, c, true))) ;
        attempt (pipe3 identifier (symbol COLON >>. typeSpec) (symbol EQ >>. expression)
            (fun a b c -> Ast.VariableDeclaration (a, b, c, false))) ;
        attempt (pipe2 identifier (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> Ast.VariableDeclaration (a, b, Ast.Empty, true))) ;
        (pipe2 identifier (symbol COLON >>. typeSpec) 
            (fun a b -> Ast.VariableDeclaration (a, b, Ast.Empty, false))) ;
    ]

let variableDeclaration : Parser<Ast.Declaration, unit> = 
    choice_ws [
        attempt (pipe3 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) (symbol EQ >>. expression)
            (fun a b c -> Ast.VariableDeclaration (a, b, c, true))) ;
        attempt (pipe3 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) (symbol EQ >>. expression)
            (fun a b c -> Ast.VariableDeclaration (a, b, c, false))) ;
        attempt (pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> Ast.VariableDeclaration (a, b, Ast.Empty, true))) ;
        (pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> Ast.VariableDeclaration (a, b, Ast.Empty, false))) ;
    ]

let declaration = variableDeclaration <|> functionDeclaration

let declarationList = many_ws declaration

let program = declarationList .>> eof

let parse (input : string) =
    match run program input with
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
