module MicroCosmo.SemanticAnalysis.VariableType

open MicroCosmo

type VariableType =
    {
        Type    : Ast.TypeSpec;
    }
    override x.ToString() = x.Type.ToString()

let typeOfDeclaration (_, t, _) = { Type = t; }
let simpleType t = { Type = t; }