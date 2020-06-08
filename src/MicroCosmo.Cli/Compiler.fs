module MicroCosmo.Compiler

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open Mono.Cecil
open Mono.Cecil.Cil

open MicroCosmo
open MicroCosmo.IL
open MicroCosmo.Cli
open MicroCosmo.Semantic
open MicroCosmo.ProcessorExtensions

type MethodMappingDictionary = Dictionary<string, MethodDefinition>
type FieldMappingDictionary = Dictionary<ILVariable, FieldDefinition>

type MethodGenerator(cache: EmitReferenceCache,
                     ilMethod : ILMethod,
                     methodMappings : MethodMappingDictionary,
                     fieldMappings : FieldMappingDictionary) =

    let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
    let returnTypeReference = cache.GetType(ilMethod.ReturnType)
    let methodBuilder = MethodDefinition(ilMethod.Name, methodAttributes, returnTypeReference)// typeBuilder.DefineMethod(ilMethod.Name, methodAttributes)

    do methodMappings.Add(ilMethod.Name, methodBuilder)

    let ilGenerator = methodBuilder.Body.GetILProcessor()
    let labelMappings = new Dictionary<ILLabel, Instruction>()

    let getLabel ilLabel =
        if labelMappings.ContainsKey ilLabel then
            labelMappings.[ilLabel]
        else
            let label = ilGenerator.DefineLabel()
            labelMappings.Add(ilLabel, label)
            label

    let getMethodReference (methodInfo: MethodInfo) =
        let ref = MethodReference(methodInfo.Name, cache.GetType(methodInfo.ReturnType), cache.GetType(methodInfo.DeclaringType))
        methodInfo.GetParameters()
        |> Seq.iter (fun b -> ref.Parameters.Add(ParameterDefinition(cache.GetType b.ParameterType)))
        ref

    let emitOpCode  = function
        | ILOpCode.Add        -> ilGenerator.Emit(OpCodes.Add)
        | ILOpCode.Br(l)      -> ilGenerator.Emit(OpCodes.Br, getLabel l)
        | ILOpCode.Brfalse(l) -> ilGenerator.Emit(OpCodes.Brfalse, getLabel l)
        | ILOpCode.Brtrue(l)  -> ilGenerator.Emit(OpCodes.Brtrue, getLabel l)
        | ILOpCode.Call(n)    -> ilGenerator.Emit(OpCodes.Call, methodMappings.[n])
        | ILOpCode.CallClr(m) -> ilGenerator.Emit(OpCodes.Call, getMethodReference m)
        | ILOpCode.Ceq        -> ilGenerator.Emit(OpCodes.Ceq)
        | ILOpCode.Cge        -> ilGenerator.Emit(OpCodes.Clt)
                                 ilGenerator.Emit(OpCodes.Ldc_I4_0)
                                 ilGenerator.Emit(OpCodes.Ceq)
        | ILOpCode.Cgt        -> ilGenerator.Emit(OpCodes.Cgt)
        | ILOpCode.Cle        -> ilGenerator.Emit(OpCodes.Cgt)
                                 ilGenerator.Emit(OpCodes.Ldc_I4_0)
                                 ilGenerator.Emit(OpCodes.Ceq)
        | ILOpCode.Clt        -> ilGenerator.Emit(OpCodes.Clt)
        | ILOpCode.Dup        -> ilGenerator.Emit(OpCodes.Dup)
        | ILOpCode.Div        -> ilGenerator.Emit(OpCodes.Div)
        | ILOpCode.Label(l)   -> ilGenerator.MarkLabel(getLabel l)
        | ILOpCode.Ldarg(i)   -> ilGenerator.Emit(OpCodes.Ldarg, int i)
        | ILOpCode.Ldstr(i)   -> ilGenerator.Emit(OpCodes.Ldstr, i)
        | ILOpCode.Ldc_I4(i)  -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
        | ILOpCode.Ldc_R8(r)  -> ilGenerator.Emit(OpCodes.Ldc_R8, r)
        | ILOpCode.Ldelem(t)  -> ilGenerator.Emit(OpCodes.Ldelema, cache.GetType t)
        | ILOpCode.Ldlen      -> ilGenerator.Emit(OpCodes.Ldlen)
        | ILOpCode.Ldloc(i)   -> ilGenerator.Emit(OpCodes.Ldloc, int i)
        | ILOpCode.Ldsfld(v)  -> ilGenerator.Emit(OpCodes.Ldsfld, fieldMappings.[v])
        | ILOpCode.Mul        -> ilGenerator.Emit(OpCodes.Mul)
        | ILOpCode.Neg        -> ilGenerator.Emit(OpCodes.Neg)
        | ILOpCode.Newarr(t)  -> ilGenerator.Emit(OpCodes.Newarr, cache.GetType t)
        | ILOpCode.Pop        -> ilGenerator.Emit(OpCodes.Pop)
        | ILOpCode.Rem        -> ilGenerator.Emit(OpCodes.Rem)
        | ILOpCode.Ret        -> ilGenerator.Emit(OpCodes.Ret)
        | ILOpCode.Starg(i)   -> ilGenerator.Emit(OpCodes.Starg, int i)
        | ILOpCode.Stelem(t)  -> ilGenerator.Emit(OpCodes.Stelem_Any, cache.GetType t)
        | ILOpCode.Stloc(i)   -> ilGenerator.Emit(OpCodes.Stloc, int i)
        | ILOpCode.Stsfld(v)  -> ilGenerator.Emit(OpCodes.Stsfld, fieldMappings.[v])
        | ILOpCode.Sub        -> ilGenerator.Emit(OpCodes.Sub)
        | ILOpCode.Conv_i4    -> ilGenerator.Emit(OpCodes.Conv_I4)
        | ILOpCode.Conv_r8    -> ilGenerator.Emit(OpCodes.Conv_R8)

    member x.Generate() =
        methodBuilder.ReturnType <- cache.GetType(ilMethod.ReturnType)
        let defineParameter name type' =
            let paramDef = ParameterDefinition(name, ParameterAttributes.In, cache.GetType type')
            methodBuilder.Parameters.Add(paramDef) |> ignore

        ilMethod.Parameters |> List.iter (fun p -> defineParameter p.Name p.Type)

        let emitLocal (variable : ILVariable) =
            let varDef = VariableDefinition(cache.GetType variable.Type)
            methodBuilder.Body.Variables.Add(varDef) |> ignore

        ilMethod.Locals |> List.iter emitLocal
        ilMethod.Body |> List.iter emitOpCode

        let rec last =
            function
            | head :: [] -> head
            | head :: tail -> last tail
            | x -> failwith "Empty list."

        if Seq.length ilMethod.Body = 0 || last ilMethod.Body <> Ret then
            ilGenerator.Emit(OpCodes.Ret)

        methodBuilder

type CodeGenerator(assemblyBuilder: AssemblyDefinition, cache: EmitReferenceCache, ilClass : ILClass, namespace' : string) =
    let fieldMappings = new FieldMappingDictionary()

    let generateField (typeBuilder : TypeDefinition) (ilField : ILVariable) =
        let fieldAttributes = FieldAttributes.Public ||| FieldAttributes.Static
        let fieldTypeRef = cache.GetType(ilField.Type)
        let fieldDecl = FieldDefinition(ilField.Name, fieldAttributes, fieldTypeRef)
        typeBuilder.Fields.Add(fieldDecl)
        fieldMappings.Add(ilField, fieldDecl)

    member x.GenerateType() =
        let typeAttributes = TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public
        let typeBuilder = TypeDefinition(namespace', "Program", typeAttributes)

        ilClass.Fields |> List.iter (generateField typeBuilder)

        let methodMappings = new MethodMappingDictionary()
        let generateMethod ilMethod =
            let methodGenerator = MethodGenerator(cache, ilMethod, methodMappings, fieldMappings)
            methodGenerator.Generate()

        ilClass.Methods |> List.map generateMethod |> List.iter (typeBuilder.Methods.Add)

        (typeBuilder, typeBuilder.Methods |> Seq.filter (fun m -> m.Name = "main") |> Seq.head)

let compile (assemblyBuilder : AssemblyDefinition) (cache: EmitReferenceCache) code =
    let assemblyName = assemblyBuilder.Name.Name

    let program = Parser.parse code
    match program with
    | Error someError -> raise someError
    | Ok someProgram ->
        let semanticAnalysisResult = analyze someProgram
        match semanticAnalysisResult with
        | Error someError -> raise someError
        | Ok someSemanticAnalysisResult ->
            let ilBuilder = ILBuilder(someSemanticAnalysisResult)
            let ilClass = ilBuilder.BuildClass someProgram
            let codeGenerator = CodeGenerator(assemblyBuilder, cache, ilClass, assemblyName)
            let (compiledType, entryPoint) = codeGenerator.GenerateType()
            compiledType.BaseType <- cache.GetType(typeof<Object>)
            (compiledType, entryPoint)

let createReferenceCache (asm: AssemblyDefinition) =
    let cache = EmitReferenceCache(asm)
    cache.AddType(typeof<Console>)
    cache.AddType(typeof<Convert>)

    let ass = Assembly.Load(AssemblyName("mscorlib"))
    cache.AddType(ass.GetType("System.Object"))
    cache.AddType(ass.GetType("System.String"))
    cache.AddType(ass.GetType("System.Double"))
    cache.AddType(ass.GetType("System.Void"))
    cache.AddType(ass.GetType("System.Boolean"))
    cache.AddType(ass.GetType("System.Int32"))

    cache

let compileToMemory assemblyName code =
    let assemblyNameDefinition = AssemblyNameDefinition(assemblyName, Version(1, 0, 0, 0))
    let asm = AssemblyDefinition.CreateAssembly(assemblyNameDefinition, assemblyName, ModuleKind.Console)
    let cache = createReferenceCache asm

    compile asm cache code

let compileToFile (fileName: string) code =
    let assemblyName = Path.GetFileNameWithoutExtension fileName
    let assemblyNameDefinition = AssemblyNameDefinition(assemblyName, Version(1, 0, 0, 0))
    let asm = AssemblyDefinition.CreateAssembly(assemblyNameDefinition, sprintf "%s.exe" assemblyName, ModuleKind.Console)
    let cache = createReferenceCache asm

    let (type', entryPoint) = compile asm cache code
    asm.MainModule.Types.Add(type') |> ignore
    asm.EntryPoint <- entryPoint
    let a = Path.GetFileName fileName
    asm.Write(a)