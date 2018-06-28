module MicroCosmo.FunctionTable

open MicroCosmo
open MicroCosmo.CompilerErrors
open MicroCosmo.VariableType
open MicroCosmo.FunctionTableEntry
open System.Collections.Generic

type FunctionTable(program) as self =
    inherit Dictionary<Ast.Identifier, FunctionTableEntry>()
    
    let rec scanDeclaration =
        function
        | Ast.VariableDeclarationStatement(x) -> ()
        | Ast.FunctionDeclarationStatement(i, p, t, _) ->
            if self.ContainsKey i then
                raise (functionAlreadyDefined i)
            self.Add(i, { ReturnType = t; ParameterTypes = List.map typeOfDeclaration p; })
    
    do
    
        // Built-in functions
        self.Add("print",       { ReturnType = Ast.NoneType;    ParameterTypes = [{ Type = Ast.Any; IsArray = false }]; })
        self.Add("println",     { ReturnType = Ast.NoneType;    ParameterTypes = [{ Type = Ast.Any; IsArray = false }]; })
        self.Add("readstr",     { ReturnType = Ast.String;      ParameterTypes = []; })
        self.Add("readint",     { ReturnType = Ast.Int;         ParameterTypes = []; })
        self.Add("readreal",    { ReturnType = Ast.Double;      ParameterTypes = []; })
        
        program |> List.iter scanDeclaration 