module MicroCosmo.FunctionTableEntry

open MicroCosmo
open MicroCosmo.VariableType

type FunctionTableEntry =
    {
        ReturnType     : Ast.TypeSpec;
        ParameterTypes : VariableType list;
    }