module rec MicroCosmo.Parser

open FParsec
open MicroCosmo.Terminants
open ParserHelpers

let parse (input : string) =
    match run program input with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let program = many_ws declarationList

// Non terminal productions
#nowarn "40"

let declarationList = many_ws declaration

let declaration = variableDeclaration <|> functionDeclaration

let variableDeclaration : Parser<Ast.Declaration, unit> = 
    choice_ws [
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> Ast.VariableDeclaration (a, b, Ast.Empty, false)) ;
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> Ast.VariableDeclaration (a, b, Ast.Empty, true)) ;
        pipe3 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) (symbol EQ >>. expression)
            (fun a b c -> Ast.VariableDeclaration (a, b, c, false)) ;
        pipe3 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) (symbol EQ >>. expression) 
            (fun a b c -> Ast.VariableDeclaration (a, b, c, true)) ;
    ]

let functionDeclaration : Parser<Ast.Declaration, unit> = 
    choice_ws [
        pipe3 (keyword FUNC >>. identifier) (symbol OPENPAREN >>. parameters .>> symbol CLOSEPAREN)
            (blockStatement)
            (fun a b c -> Ast.FunctionDeclaration (a, b, Ast.None, c)) ;
        pipe4 (keyword FUNC >>. identifier) (symbol OPENPAREN >>. parameters .>> symbol CLOSEPAREN)
            (symbol COLON >>. typeSpec) (blockStatement)
            (fun a b c d -> Ast.FunctionDeclaration (a, b, c, d)) ;
    ]

let identifier : Parser<Ast.Identifier, unit> = 
    regex_ws IDENTIFIER |>> (fun a -> string a)

let typeSpec : Parser<Ast.TypeSpec, unit> = 
    choice_ws [
        keyword NONE    |>> (fun _ -> Ast.None) ;
        keyword ANY     |>> (fun _ -> Ast.Any) ;
        keyword STRING  |>> (fun _ -> Ast.String) ;
        keyword INT     |>> (fun _ -> Ast.Int) ;
        keyword DOUBLE  |>> (fun _ -> Ast.Double) ;
        keyword BOOL    |>> (fun _ -> Ast.Bool) ;
    ]


let parameters : Parser<Ast.Parameters, unit> = sepBy_ws parameter (symbol COMMA)

let parameter : Parser<Ast.VariableDeclaration, unit> =
    choice_ws [
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> (a, b, Ast.Empty, false)) ;
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> (a, b, Ast.Empty, true)) ;
    ]
    
let statements : Parser<Ast.Statement list, unit> = many_ws statement

let statement : Parser<Ast.Statement, unit> =
    choice_ws [
        expressionStatement ;
        blockStatement ;
        ifStatement ;
        whileStatement ;
        returnStatement ;
        breakStatement ;
    ]
    
let expressionStatement : Parser<Ast.Statement, unit> =
    expression |>> (fun a -> Ast.ExpressionStatement (a))

let whileStatement : Parser<Ast.Statement, unit> =
    pipe2 (keyword WHILE >>. expression) (symbol OPENCURLY >>. statement .>> symbol CLOSECURLY) 
        (fun a b -> Ast.WhileStatement (a, b))

let blockStatement : Parser<Ast.Statement, unit> =
    pipe2 (symbol OPENCURLY >>. localDeclarations) (statements .>> symbol CLOSECURLY) 
        (fun a b -> Ast.BlockStatement (a, b))

let localDeclarations : Parser<Ast.VariableDeclaration list, unit> = 
    many_ws localDeclaration

let localDeclaration : Parser<Ast.VariableDeclaration, unit> = 
    choice_ws [
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec) 
            (fun a b -> (a, b, Ast.Empty, false)) ;
        pipe2 (keyword LET >>. identifier) (symbol COLON >>. typeSpec .>> keyword ARRAY) 
            (fun a b -> (a, b, Ast.Empty, true)) ;
    ]
    
let ifStatement : Parser<Ast.Statement, unit> =
    choice_ws [
        pipe2 (keyword IF >>. expression) (statement) 
            (fun a b -> Ast.IfStatement (a, b, None)) ;
        pipe3 (keyword IF >>. expression) (statement) 
            (keyword ELSE >>. statement)
            (fun a b c -> Ast.IfStatement (a, b, Some c)) ;
    ]
    
let returnStatement : Parser<Ast.Statement, unit> = 
    choice_ws [
        (keyword RETURN |>> (fun _ -> Ast.ReturnStatement None))
        (keyword RETURN >>. expression |>> (fun a -> Ast.ReturnStatement (Some a)))
    ]
    
let breakStatement : Parser<Ast.Statement, unit> = 
    (keyword BREAK |>> (fun _ -> Ast.BreakStatement))
    
// TODO: 
let expression : Parser<Ast.Expression, unit> =
    keyword BREAK |>> (fun _ -> Ast.IdentifierExpression ({ Identifier = "lolkek" }))










let pStringLiteral = 
    literal STRING_LIT                     
        |>> (fun s -> Ast.StringLiteral((s.Substring(1, s.Length - 2))))

let boolLiteral = 
    let trueLiteral = str_ws TRUE_LIT      
                        |>> (fun _ -> Ast.BoolLiteral true)
                        
    let falseLiteral = str_ws FALSE_LIT    
                        |>> (fun _ -> Ast.BoolLiteral false)
                        
    trueLiteral <|> falseLiteral
    
let intLiteral =
    literal INT_LIT 
        |>> (fun a -> Ast.IntLiteral (int a))

let doubleLiteral =
    literal DOUBLE_LIT 
        |>> (fun a -> Ast.DoubleLiteral (double a))

let arrayLiteral =
    let typeSpec_ws = typeSpec .>> ws
    let array_ws = regex_ws ARRAY .>> ws
    (typeSpec_ws .>> array_ws) .>>. expression 
        |>> (fun (a, b) -> Ast.ArrayAllocationExpression (a, b))



// let parse (input : string) = ws >>. parseProgram .>> eof