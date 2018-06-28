module MicroCosmo.SemanticAnalysis.FunctionTableEntry

open MicroCosmo
open MicroCosmo.SemanticAnalysis.VariableType

type FunctionTableEntry =
    {
        ReturnType     : Ast.TypeSpec;
        ParameterTypes : VariableType list;
    }