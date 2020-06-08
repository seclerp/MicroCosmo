namespace MicroCosmo.Cli

open System
open System.Collections.Generic
open Mono.Cecil

type EmitReferenceCache(asm: AssemblyDefinition) =
    let typeReferenceCache = Dictionary<Type, TypeReference>()

    member x.AddType(type': Type) =
        typeReferenceCache.Add(type', asm.MainModule.ImportReference(type'))

    member x.GetType(type': Type) =
        typeReferenceCache.[type']