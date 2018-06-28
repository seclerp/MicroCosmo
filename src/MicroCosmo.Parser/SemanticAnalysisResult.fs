module MicroCosmo.SemanticAnalysisResult

open MicroCosmo.SymbolTable
open MicroCosmo.ExpressionTypeTable
open MicroCosmo.FunctionTable

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeTable;
    }