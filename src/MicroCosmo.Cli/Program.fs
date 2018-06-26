open System
open MicroCosmo.Parser

[<EntryPoint>]
let main argv =
    printfn "MicroCosmo interpreter"
    printfn "You are in AST debug mode\n"
    let rec listen() =
        printf "> "
        let input = Console.ReadLine()
        try 
            parse input
            listen()
            //printfn "%A" result
        with
        | _ as ex -> printfn "%A" ex
    listen()
    0 // return an integer exit code
