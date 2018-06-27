module MicroCosmo.SymbolTable

open MicroCosmo
open MicroCosmo.SymbolScopeStack
open MicroCosmo.VariableType
open MicroCosmo.CompilerErrors

open System
open System.Collections.Generic

type SymbolTable(program) as self =
    inherit Dictionary<Ast.IdentifierRef, Ast.VariableDeclaration>(HashIdentity.Reference)
    
    let whileStatementStack = Stack<Ast.WhileStatement>()
    let symbolScopeStack = new SymbolScopeStack() 

    let rec scanDeclaration =
        function
        | Ast.VariableDeclaration(x) -> symbolScopeStack.AddDeclaration x
        | Ast.FunctionDeclaration(x) -> scanFunctionDeclaration x
        
    and scanFunctionDeclaration (_, parameters, functionReturnType, blockStatement) =
        let rec scanBlockStatement (localDeclarations, statements) =
            symbolScopeStack.Push()
            localDeclarations |> List.iter (fun d -> symbolScopeStack.AddDeclaration d)
            statements |> List.iter scanStatement
            symbolScopeStack.Pop() |> ignore 

        and scanStatement =
            function
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
            
        and scanExpression =
            function
            | Ast.VariableAssignmentExpression(i, e) ->
                addIdentifierMapping i
                scanExpression e
            | Ast.ArrayVariableAssignmentExpression(i, e1, e2) ->
                addIdentifierMapping i
                scanExpression e1
                scanExpression e2
            | Ast.BinaryExpression(e1, _, e2) ->
                scanExpression e1
                scanExpression e2
            | Ast.UnaryExpression(_, e) ->
                scanExpression e
            | Ast.IdentifierExpression(i) ->
                addIdentifierMapping i
            | Ast.ArrayIdentifierExpression(i, e) ->
                addIdentifierMapping i
                scanExpression e
            | Ast.FunctionCallExpression(_, args) ->
                args |> List.iter scanExpression
            | Ast.ArraySizeExpression(i) ->
                addIdentifierMapping i
            | Ast.LiteralExpression(l) -> ()
            | Ast.ArrayAllocationExpression(_, e) ->
                scanExpression e 

        let toBlockStatement = 
            function 
            | Ast.BlockStatement x -> x
            | _ -> failwithf "Some strange things happen"

        symbolScopeStack.Push()
        parameters |> List.iter symbolScopeStack.AddDeclaration
        scanBlockStatement (toBlockStatement blockStatement)
        symbolScopeStack.Pop() |> ignore 
        
    do program |> List.iter scanDeclaration
        
    member x.GetIdentifierTypeSpec identifierRef =
        typeOfDeclaration self.[identifierRef] 