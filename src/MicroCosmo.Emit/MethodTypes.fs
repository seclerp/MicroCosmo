module MicroCosmo.Emit.MethodTypes

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

open MicroCosmo.IL

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
        
    member x.Generate() =
        methodBuilder.SetReturnType ilMethod.ReturnType
        methodBuilder.SetParameters (List.toArray (ilMethod.Parameters |> List.map (fun p -> p.Type)))
            
        let defineParameter index name = 
            methodBuilder.DefineParameter(index, ParameterAttributes.In, name) |> ignore
            
        ilMethod.Parameters |> List.iteri (fun i p -> defineParameter (i + 1) p.Name)
        
        let emitLocal (ilGenerator : ILGenerator) variable =
            ilGenerator.DeclareLocal(variable.Type).SetLocalSymInfo(variable.Name)
            
        ilMethod.Locals |> List.iter (emitLocal ilGenerator)
        ilMethod.Body |> List.iter (emitOpCode ilGenerator)
        
        let rec last =
            function
            | head :: [] -> head
            | head :: tail -> last tail
            | _ -> failwith "Empty list."
            
        if (last ilMethod.Body) <> Ret then
            ilGenerator.Emit(OpCodes.Ret)