module Compiler

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

open IL
open Semantic

type MethodMappingDictionary = Dictionary<string, MethodInfo>
type FieldMappingDictionary = Dictionary<ILVariable, FieldInfo>

type MethodGenerator(typeBuilder : TypeBuilder, ilMethod : ILMethod,
                     methodMappings : MethodMappingDictionary,
                     fieldMappings : FieldMappingDictionary) =
                     
    let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
    let methodBuilder = typeBuilder.DefineMethod(ilMethod.Name, methodAttributes)
    
    do methodMappings.Add(ilMethod.Name, methodBuilder)
    
    let ilGenerator = methodBuilder.GetILGenerator()
    let labelMappings = new Dictionary<ILLabel, System.Reflection.Emit.Label>()
    
    let getLabel ilLabel =
        if labelMappings.ContainsKey ilLabel then
            labelMappings.[ilLabel]
        else
            let label = ilGenerator.DefineLabel()
            labelMappings.Add(ilLabel, label)
            label
            
    let emitOpCode (ilGenerator : ILGenerator) = function
        | ILOpCode.Add        -> ilGenerator.Emit(OpCodes.Add)
        | ILOpCode.Br(l)      -> ilGenerator.Emit(OpCodes.Br, getLabel l)
        | ILOpCode.Brfalse(l) -> ilGenerator.Emit(OpCodes.Brfalse, getLabel l)
        | ILOpCode.Brtrue(l)  -> ilGenerator.Emit(OpCodes.Brtrue, getLabel l)
        | ILOpCode.Call(n)    -> ilGenerator.Emit(OpCodes.Call, methodMappings.[n])
        | ILOpCode.CallClr(m) -> ilGenerator.Emit(OpCodes.Call, m)
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
        | ILOpCode.Ldarg(i)   -> ilGenerator.Emit(OpCodes.Ldarg, i)
        | ILOpCode.Ldstr(i)   -> ilGenerator.Emit(OpCodes.Ldstr, i)
        | ILOpCode.Ldc_I4(i)  -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
        | ILOpCode.Ldc_R8(r)  -> ilGenerator.Emit(OpCodes.Ldc_R8, r)
        | ILOpCode.Ldelem(t)  -> ilGenerator.Emit(OpCodes.Ldelem, t)
        | ILOpCode.Ldlen      -> ilGenerator.Emit(OpCodes.Ldlen)
        | ILOpCode.Ldloc(i)   -> ilGenerator.Emit(OpCodes.Ldloc, i)
        | ILOpCode.Ldsfld(v)  -> ilGenerator.Emit(OpCodes.Ldsfld, fieldMappings.[v])
        | ILOpCode.Mul        -> ilGenerator.Emit(OpCodes.Mul)
        | ILOpCode.Neg        -> ilGenerator.Emit(OpCodes.Neg)
        | ILOpCode.Newarr(t)  -> ilGenerator.Emit(OpCodes.Newarr, t)
        | ILOpCode.Pop        -> ilGenerator.Emit(OpCodes.Pop)
        | ILOpCode.Rem        -> ilGenerator.Emit(OpCodes.Rem)
        | ILOpCode.Ret        -> ilGenerator.Emit(OpCodes.Ret)
        | ILOpCode.Starg(i)   -> ilGenerator.Emit(OpCodes.Starg, i)
        | ILOpCode.Stelem(t)  -> ilGenerator.Emit(OpCodes.Stelem, t)
        | ILOpCode.Stloc(i)   -> ilGenerator.Emit(OpCodes.Stloc, i)
        | ILOpCode.Stsfld(v)  -> ilGenerator.Emit(OpCodes.Stsfld, fieldMappings.[v])
        | ILOpCode.Sub        -> ilGenerator.Emit(OpCodes.Sub)
        | ILOpCode.Conv_i4    -> ilGenerator.Emit(OpCodes.Conv_I4)
        | ILOpCode.Conv_r8    -> ilGenerator.Emit(OpCodes.Conv_R8)
        
    member x.Generate() =
        methodBuilder.SetReturnType ilMethod.ReturnType
        methodBuilder.SetParameters (List.toArray (ilMethod.Parameters |> List.map (fun p -> p.Type)))
            
        let defineParameter index name = 
            methodBuilder.DefineParameter(index, ParameterAttributes.In, name) |> ignore
            
        ilMethod.Parameters |> List.iteri (fun i p -> defineParameter (i + 1) p.Name)
        
        let emitLocal (ilGenerator : ILGenerator) (variable : ILVariable)=
            ilGenerator.DeclareLocal(variable.Type).SetLocalSymInfo(variable.Name)
            
        ilMethod.Locals |> List.iter (emitLocal ilGenerator)
        ilMethod.Body |> List.iter (emitOpCode ilGenerator)
        let rec last =
            function
            | head :: [] -> head
            | head :: tail -> last tail
            | x -> failwith "Empty list."
            
        if Seq.length ilMethod.Body = 0 || last ilMethod.Body <> Ret then
            ilGenerator.Emit(OpCodes.Ret)

type CodeGenerator(moduleBuilder : ModuleBuilder, ilClass : ILClass, moduleName : string) =
    let fieldMappings = new FieldMappingDictionary()
    
    let generateField (typeBuilder : TypeBuilder) (ilField : ILVariable) =
        let fieldAttributes = FieldAttributes.Public ||| FieldAttributes.Static
        let fieldBuilder = typeBuilder.DefineField(ilField.Name, ilField.Type, fieldAttributes)
        fieldMappings.Add(ilField, fieldBuilder)
        
    member x.GenerateType() =
        let typeAttributes = TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public
        let typeBuilder = moduleBuilder.DefineType(moduleName + ".Program", typeAttributes)
        
        ilClass.Fields |> List.iter (generateField  typeBuilder)
        
        let methodMappings = new MethodMappingDictionary()
        let generateMethod ilMethod =
            let methodGenerator = new MethodGenerator(typeBuilder, ilMethod, methodMappings, fieldMappings)
            methodGenerator.Generate()
        ilClass.Methods |> List.iter generateMethod
        
        (typeBuilder.CreateType(), typeBuilder.GetMethod("main"))
        
let compile (assemblyBuilder : AssemblyBuilder) code =
    let assemblyName = assemblyBuilder.GetName()
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, assemblyName.Name + ".exe", true)
    
    let program = Parser.parse code
    match program with
    | Error someError -> raise someError
    | Ok someProgram -> 
        let semanticAnalysisResult = analyze someProgram
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
        
    compile assemblyBuilder code |> ignore
    assemblyBuilder.Save (Path.GetFileName fileName)