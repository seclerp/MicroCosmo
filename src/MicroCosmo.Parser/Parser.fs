module MicroCosmo.Parser

open FParsec
open MicroCosmo.Terminants

// Helpers
let ws = spaces
let str = string
let str_ws s = pstring s .>> ws
let regex_ws s = regex s .>> ws
let many_ws s = many s .>> ws
let choice_ws s = choice s .>> ws

let keyword s = str_ws s
let operator s = str_ws s
let literal s = regex_ws s

// Non terminal productions
let rec program = many_ws declarationList

and declarationList = many_ws pDeclaration

and declaration = choice_ws [ variableDeclaration; functionDeclaration ]

and variableDeclaration = [
        keyword LET >>. ident .>> ws .>> operator COLON .>>. typeSpec |>> (fun ab -> Ast.ScalarVariableDeclaration ab);
        keyword LET >>. ident .>> ws .>> operator COLON .>>. typeSpec .>> keyword ARRAY |>> (fun ab -> Ast.ArrayVariableDeclaration ab);
        keyword LET >>. ident .>> ws .>> operator COLON .>>. typeSpec .>> operator EQ .>>. expression |>> (fun ab -> Ast.ScalarVariableDeclaration ab);
        keyword LET >>. ident .>> ws .>> operator COLON .>>. typeSpec .>> keyword ARRAY .>> operator EQ .>>. expression |>> (fun ab -> Ast.ArrayVariableDeclaration ab);
    ]

let pStringLiteral = 
    literal STRING_LIT                     
        |>> (fun s -> Ast.StringLiteral((s.Substring(1, s.Length - 2))))

let pBoolLiteral = 
    let pTrueLiteral = str_ws TRUE_LIT      |>> (fun _ -> Ast.BoolLiteral true)
    let pFalseLiteral = str_ws FALSE_LIT    |>> (fun _ -> Ast.BoolLiteral false)
    pTrueLiteral <|> pFalseLiteral
    
let pIntLiteral =
    literal INT_LIT                        |>> (fun a -> Ast.IntLiteral (int a))

let pDoubleLiteral =
    literal DOUBLE_LIT                     |>> (fun a -> Ast.DoubleLiteral (double a))

let pArrayLiteral =
    let typeSpec_ws = pTypeSpec .>> ws
    let array_ws = regex_ws ARRAY .>> ws
    (typeSpec_ws .>> array_ws) .>>. pExpr     |>> (fun (a, b) -> Ast.ArrayAllocationExpression (a, b))



// let parse (input : string) = ws >>. parseProgram .>> eof