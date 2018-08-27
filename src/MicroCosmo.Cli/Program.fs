open MicroCosmo
open System
open MicroCosmo.SemanticAnalyzer
open MicroCosmo.SyntaxAnalysis.Parser
open MicroCosmo.IR.ILBuilder

let InputFileKeys = ["-i"; "--input"]
let OutputFilekeys = ["-o"; "--output"]

type CommandLineOptions = {
    InputFile : string;
    OutputFile : string;
}

[<EntryPoint>]
let main argv =
    printfn "MicroCosmo interpreter"
    printfn "You are in parser debug mode\n"
    
    let processCommandLineArgs args =
        let defaultOptions = {
            InputFile = "program.mcos";
            OutputFile = "program.exe";
        }
    
        let rec processCommandLineArgsRec args (optionsSoFar : CommandLineOptions) =
            match args with
            | [] -> optionsSoFar
            | x::xs when InputFileKeys |> List.contains x -> 
                match xs with
                | [] -> 
                    printfn "You need to specify path to input file when using '-i' option" 
                    processCommandLineArgsRec xs optionsSoFar
                | x::xs -> 
                    let newOptionsSoFar = { optionsSoFar with InputFile = x }
                    processCommandLineArgsRec xs newOptionsSoFar
            | x::xs when OutputFilekeys |> List.contains x -> 
                match xs with
                | [] -> 
                    printfn "You need to specify path to output file when using '-o' option" 
                    processCommandLineArgsRec xs optionsSoFar
                | x::xs -> 
                    let newOptionsSoFar = { optionsSoFar with InputFile = x }
                    processCommandLineArgsRec xs newOptionsSoFar
            | x::xs -> 
                printfn "Unrecognized option '%s'" x
                processCommandLineArgsRec xs optionsSoFar
            
        processCommandLineArgsRec args defaultOptions
    
    let printfnColored color pattern object =
        let consoleColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        printfn pattern object
        Console.ForegroundColor <- consoleColor
    
    let options = processCommandLineArgs (argv |> Array.toList)
    
    let rec listen() =
        let consoleColor = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printf "> "
        let input = Console.ReadLine()
        Console.ForegroundColor <- consoleColor
        try 
            let parserResult = parse input
            match parserResult with
            | Error e -> 
                printfnColored (ConsoleColor.Red) "%A" e |> ignore
            | Ok p -> 
                //printfnColored (ConsoleColor.Yellow) "AST: \n\n%A\n" p
                let semanticAnalysisResult = analyze p
                match semanticAnalysisResult with
                | Error e -> 
                    printfnColored (ConsoleColor.Red) "%A" e |> ignore
                | Ok a ->
                    //printfnColored (ConsoleColor.Yellow) "Semantic analysis: \n\n%A\n" a |> ignore
                    let ilBuilder = new ILBuilder(a)
                    let irResult = ilBuilder.BuildClass p
                    
                    printfnColored (ConsoleColor.Yellow) "Intermediate representation: \n\n%A\n" irResult |> ignore
                    
            printfn ""
            listen()
            //printfn "%A" result
        with
        | _ as ex -> printfnColored (ConsoleColor.Red) "%A" ex
    listen()
    0 // return an integer exit code
