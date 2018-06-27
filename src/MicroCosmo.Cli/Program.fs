open MicroCosmo
open System
open MicroCosmo.Parser
open MicroCosmo.SemanticAnalyzer

[<EntryPoint>]
let main argv =
    printfn "MicroCosmo interpreter"
    printfn "You are in parser debug mode\n"
    let rec listen() =
        printf "> "
        let input = Console.ReadLine()
        try 
            let parserResult = parse input
            match parserResult with
            | Error e -> printfn "%A" e
            | Ok p -> 
                printfn "AST: \n\n%A\n" p
                let semanticAnalysisResult = analyze p
                match semanticAnalysisResult with
                | Error e -> printfn "%A" e
                | Ok a -> 
                    printfn "Symbol table: \n\n%A\n" a
                    
            printfn ""
            listen()
            //printfn "%A" result
        with
        | _ as ex -> printfn "%A" ex
    listen()
    0 // return an integer exit code
