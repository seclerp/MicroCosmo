module MicroCosmo.Emit.Compiler

open MicroCosmo.Emit.CodeGeneration
open MicroCosmo.IR.ILBuilder
open MicroCosmo
open MicroCosmo.SyntaxAnalysis
open MicroCosmo.IL
open System.Reflection
open System.Reflection.Emit
open System.IO
open System

let compile (assemblyBuilder : AssemblyBuilder) code =
    let assemblyName = assemblyBuilder.GetName()
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, assemblyName.Name + ".exe", true)
    
    let program = Parser.parse code
    match program with
    | Error someError -> raise someError
    | Ok someProgram -> 
        let semanticAnalysisResult = SemanticAnalyzer.analyze someProgram
        match semanticAnalysisResult with
        | Error someError -> raise someError
        | Ok someSemanticAnalysisResult -> 
            let ilBuilder = new ILBuilder(someSemanticAnalysisResult)
            let ilClass = ilBuilder.BuildClass someProgram
            let codeGenerator = new CodeGenerator(moduleBuilder, ilClass, assemblyName.Name)
            let (compiledType, entryPoint) = codeGenerator.GenerateType()
            assemblyBuilder.SetEntryPoint entryPoint
            
            (compiledType, entryPoint)
            
let compileToMemory assemblyName code =
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            assemblyName, AssemblyBuilderAccess.RunAndSave
        )
        
    compile assemblyBuilder code
    
let compileToFile fileName code =
    let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension fileName)
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            assemblyName, AssemblyBuilderAccess.RunAndSave,
            Path.GetDirectoryName(fileName)
        )
        
    let (_, _) = compile assemblyBuilder code
    assemblyBuilder.Save (Path.GetFileName fileName)