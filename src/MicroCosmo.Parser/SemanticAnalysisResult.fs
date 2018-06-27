module MicroCosmo.SemanticAnalysisResult

open MicroCosmo.SymbolTable
open MicroCosmo.ExpressionTypeTable

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeTable;
    }