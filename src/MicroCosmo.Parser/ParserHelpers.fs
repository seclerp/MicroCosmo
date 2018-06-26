module MicroCosmo.ParserHelpers

open FParsec

let ws = spaces
let str = string
let str_ws s = pstring s .>> ws
let regex_ws s = regex s .>> ws
let many_ws s = many s .>> ws
let choice_ws s = choice s .>> ws
let sepBy_ws a b = sepBy a b .>> ws

let keyword s = str_ws s
let symbol s = str_ws s
let literal s = regex_ws s

let unary (x : Ast.Expression) (y : Ast.UnaryOperator) : Ast.Expression = 
    Ast.UnaryExpression(y, x)
let binary (x : Ast.Expression) (y : Ast.BinaryOperator) (z : Ast.Expression) : Ast.Expression = 
    Ast.BinaryExpression(x, y, z)
    
/// For debugging

let BP (p: Parser<_,_>) stream =
    p stream // Set a breakpoint here
    
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply