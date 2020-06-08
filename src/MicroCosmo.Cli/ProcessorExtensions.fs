module MicroCosmo.ProcessorExtensions

open Mono.Cecil.Cil

type ILProcessor with
    member x.DefineLabel() =
        x.Create(OpCodes.Nop)

    member x.MarkLabel(labelInstruction: Instruction) =
        x.Append(labelInstruction)