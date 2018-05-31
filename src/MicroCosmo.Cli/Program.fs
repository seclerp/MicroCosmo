open System
open MicroCosmo.Parser

[<EntryPoint>]
let main argv =
    printfn "MicroCosmo interpreter"
    printfn "You are in AST debug mode\n"
    let listen =
        printf "> "
        let input = Console.ReadLine()
        try 
            let result = parse input
            printfn "%A" result
        with
        | _ as ex -> printfn "%A" ex
        
    0 // return an integer exit code
