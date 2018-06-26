module ParserHelpers

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