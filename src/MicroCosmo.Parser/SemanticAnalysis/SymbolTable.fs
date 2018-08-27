module MicroCosmo.SemanticAnalysis.SymbolTable

open MicroCosmo
open MicroCosmo.SemanticAnalysis.SymbolScopeStack
open MicroCosmo.SemanticAnalysis.VariableType
open MicroCosmo.CompilerErrors

open System
open System.Collections.Generic

type SymbolTable(program) as self =
    inherit Dictionary<Ast.IdentifierRef, Ast.VariableDeclarationStatement>()
    
//    let sameIdentifierMap = Dictionary<string, string>()
//    let sameIdentifierCounters = Dictionary<string, int>()
    let whileStatementStack = Stack<Ast.WhileStatement>()
    let symbolScopeStack = new SymbolScopeStack() 

    let rec scanDeclaration =
        function
        | Ast.VariableDeclarationStatement(x) -> symbolScopeStack.AddDeclaration x
        | Ast.FunctionDeclarationStatement(x) -> scanFunctionDeclaration x
        
    and scanFunctionDeclaration (_, parameters, functionReturnType, blockStatement, _) =
        let rec scanBlockStatement statements =
            symbolScopeStack.Push()
            statements |> List.iter scanStatement
            symbolScopeStack.Pop() |> ignore 

        and scanStatement =
            function
            | Ast.VariableDeclarationStatement(i, t, e, a, g) -> 
                match e with
                | Some e -> scanExpression e
                | None -> ()
                symbolScopeStack.AddDeclaration (i, t, e, a, g)
            | Ast.ExpressionStatement(es) ->
                match es with
                | Ast.Empty -> ()
                | e -> scanExpression e
            | Ast.BlockStatement(x) -> scanBlockStatement x
            | Ast.IfStatement(e, s1, Some(s2)) ->
                scanExpression e
                scanStatement s1
                scanStatement s2
            | Ast.IfStatement(e, s1, None) ->
                scanExpression e
                scanStatement s1
            | Ast.WhileStatement(e, s) ->
                whileStatementStack.Push (e, s)
                scanExpression e
                scanStatement s
                whileStatementStack.Pop() |> ignore
            | Ast.ReturnStatement(Some(e)) ->
                scanExpression e
            | Ast.ReturnStatement(None) ->
                if functionReturnType <> Ast.NoneType then
                    raise (cannotConvertType (Ast.NoneType.ToString()) (functionReturnType.ToString()))
            | Ast.BreakStatement ->
                if whileStatementStack.Count = 0 then
                    raise (noEnclosingLoop()) 

        and addIdentifierMapping identifierRef =
            let declaration = symbolScopeStack.CurrentScope.FindDeclaration identifierRef
            self.Add(identifierRef, declaration) 
            
        and scanExpression (e : Ast.Expression) =
            match e with
            | Ast.VariableAssignmentExpression(i, e, _) ->
                addIdentifierMapping i
                scanExpression e
            | Ast.ArrayVariableAssignmentExpression(i, e1, e2, _) ->
                addIdentifierMapping i
                scanExpression e1
                scanExpression e2
            | Ast.BinaryExpression(e1, _, e2, _) ->
                scanExpression e1
                scanExpression e2
            | Ast.UnaryExpression(_, e, _) ->
                scanExpression e
            | Ast.IdentifierExpression(i, _) ->
                addIdentifierMapping i
            | Ast.ArrayIdentifierExpression(i, e, _) ->
                addIdentifierMapping i
                scanExpression e
            | Ast.FunctionCallExpression(_, args, _) ->
                args |> List.iter scanExpression
            | Ast.ArraySizeExpression(i, _) ->
                addIdentifierMapping i
            | Ast.LiteralExpression(l, _) -> ()
            | Ast.ArrayAllocationExpression(_, e, _) ->
                scanExpression e 
            | Ast.Empty -> ()

        let toBlockStatement = function Ast.BlockStatement x -> x

        symbolScopeStack.Push()
        parameters |> List.iter symbolScopeStack.AddDeclaration
        scanBlockStatement (toBlockStatement blockStatement)
        symbolScopeStack.Pop() |> ignore 
        
    do program |> List.iter scanDeclaration
        
    member x.GetIdentifierTypeSpec identifierRef =
        typeOfDeclaration self.[identifierRef] 