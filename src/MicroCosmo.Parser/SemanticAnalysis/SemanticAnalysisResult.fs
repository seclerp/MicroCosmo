module MicroCosmo.SemanticAnalysis.SemanticAnalysisResult

open MicroCosmo.SemanticAnalysis.SymbolTable
open MicroCosmo.ExpressionTypeTable
open MicroCosmo.SemanticAnalysis.FunctionTable

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeTable;
    }