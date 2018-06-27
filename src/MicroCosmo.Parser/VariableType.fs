module MicroCosmo.VariableType

open MicroCosmo

type VariableType =
    {
        Type    : Ast.TypeSpec;
        IsArray : bool;
    }
    override x.ToString() =
        (if x.IsArray then "array of" else "") + x.Type.ToString()

let typeOfDeclaration (_, t, _, a) = { Type = t; IsArray = a }