module MicroCosmo.Emit.CodeGeneration

open MicroCosmo.Emit.MethodTypes
open MicroCosmo.IL
open System.Reflection
open System.Reflection.Emit

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