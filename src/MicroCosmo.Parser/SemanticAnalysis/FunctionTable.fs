module MicroCosmo.SemanticAnalysis.FunctionTable

open MicroCosmo
open MicroCosmo.CompilerErrors
open MicroCosmo.SemanticAnalysis.VariableType
open MicroCosmo.SemanticAnalysis.FunctionTableEntry
open System.Collections.Generic

type FunctionTable(program) as self =
    inherit Dictionary<Ast.Identifier, FunctionTableEntry>()
    
    let rec scanDeclaration =
        function
        | Ast.FunctionDeclarationStatement(i, p, t, _, _) ->
            if self.ContainsKey i then
                raise (functionAlreadyDefined i)
            self.Add(i, { ReturnType = t; ParameterTypes = List.map typeOfDeclaration p; })
        | _ -> ()
    do
        // Built-in functions
        self.Add("print",       { ReturnType = Ast.NoneType;    ParameterTypes = [{ Type = Ast.String; }]; })
        self.Add("println",     { ReturnType = Ast.NoneType;    ParameterTypes = [{ Type = Ast.String; }]; })
        self.Add("readstr",     { ReturnType = Ast.String;      ParameterTypes = []; })
        self.Add("readint",     { ReturnType = Ast.Int;         ParameterTypes = []; })
        self.Add("readreal",    { ReturnType = Ast.Double;      ParameterTypes = []; })
        self.Add("itostr",      { ReturnType = Ast.String;      ParameterTypes = [{ Type = Ast.Int }]; })
        self.Add("dtostr",      { ReturnType = Ast.String;      ParameterTypes = [{ Type = Ast.Double }]; })
        self.Add("itod",        { ReturnType = Ast.Int;         ParameterTypes = [{ Type = Ast.Double }]; })
        self.Add("dtoi",        { ReturnType = Ast.Double;      ParameterTypes = [{ Type = Ast.Int }]; })
        
        program |> List.iter scanDeclaration 