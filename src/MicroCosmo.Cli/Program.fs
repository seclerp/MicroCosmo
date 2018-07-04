open MicroCosmo
open System
open MicroCosmo.SemanticAnalyzer
open MicroCosmo.SyntaxAnalysis.Parser
open MicroCosmo.IR.ILBuilder

[<EntryPoint>]
let main argv =
    printfn "MicroCosmo interpreter"
    printfn "You are in parser debug mode\n"
    
    let printfnColored color pattern object =
        let consoleColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        printfn pattern object
        Console.ForegroundColor <- consoleColor
    
    let rec listen() =
        let consoleColor = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printf "> "
        let input = Console.ReadLine()
        Console.ForegroundColor <- consoleColor
        try 
            let parserResult = parse input
            match parserResult with
            | Error e -> printfnColored (ConsoleColor.Red) "%A" e |> ignore
            | Ok p -> 
                printfnColored (ConsoleColor.Yellow) "AST: \n\n%A\n" p
                let semanticAnalysisResult = analyze p
                match semanticAnalysisResult with
                | Error e ->    printfnColored (ConsoleColor.Red) "%A" e |> ignore
                | Ok a ->       
                    printfnColored (ConsoleColor.Yellow) "Semantic analysis: \n\n%A\n" a |> ignore
                    let ilBuilder = new ILBuilder(a)
                    let irResult = ilBuilder.BuildClass p
                    
                    printfnColored (ConsoleColor.Yellow) "Intermediate representation: \n\n%A\n" irResult |> ignore
                    
            printfn ""
            listen()
            //printfn "%A" result
        with
        | _ as ex -> printfn "%A" ex
    listen()
    0 // return an integer exit code
