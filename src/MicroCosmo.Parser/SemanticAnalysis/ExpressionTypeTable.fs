module MicroCosmo.ExpressionTypeTable

open System.Collections.Generic
open MicroCosmo
open MicroCosmo.SemanticAnalysis.SymbolTable
open MicroCosmo.SemanticAnalysis.FunctionTable
open MicroCosmo.SemanticAnalysis.VariableType
open MicroCosmo.CompilerErrors
open System

type ExpressionTypeTable(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Ast.Expression, VariableType>(HashIdentity.Reference)
    
    let rec scanDeclaration =
        function
        | Ast.FunctionDeclarationStatement(x) -> scanFunctionDeclaration x
        | Ast.VariableDeclarationStatement(i, t, e, a) -> 
            match e with
            | Some ex -> 
                let typeOfE = scanExpression ex
                let typeOfI = { Type = t; IsArray = a; }
                checkCast typeOfE typeOfI
            | None -> ()
    
    and scanFunctionDeclaration (_, _, functionReturnType, blockStatement) =
        let rec scanBlockStatement statements =
            statements |> List.iter scanStatement 
            
        and scanStatement =
            function
            | Ast.ExpressionStatement(es) ->
                match es with
                | Ast.Empty -> ()
                | _ -> scanExpression es |> ignore
            | Ast.BlockStatement(x) -> scanBlockStatement x
            | Ast.IfStatement(e, s1, Some(s2)) ->
                scanExpression e |> ignore
                scanStatement s1
                scanStatement s2
            | Ast.IfStatement(e, s1, None) ->
                scanExpression e |> ignore
                scanStatement s1
            | Ast.WhileStatement(e, s) ->
                scanExpression e |> ignore
                scanStatement s
            | Ast.ReturnStatement(Some(e)) ->
                let typeOfE = scanExpression e
                checkCast typeOfE (simpleType functionReturnType)
            | Ast.VariableDeclarationStatement(i, t, e, a) -> 
                match e with
                | Some ex -> 
                    let typeOfE = scanExpression ex
                    let typeOfI = { Type = t; IsArray = a; }
                    checkCast typeOfE typeOfI
                | None -> ()
            | _ -> () 
                       
        let toBlockStatement = function Ast.BlockStatement x -> x
        scanBlockStatement (toBlockStatement blockStatement)
        
    and scanExpression expression =
        let checkArrayIndexType e =
            let arrayIndexType = scanExpression e
            if arrayIndexType <> simpleType Ast.Int then
                raise (cannotConvertType (arrayIndexType.ToString()) (Ast.Int.ToString()))
       
        let expressionType =
            match expression with
            | Ast.VariableAssignmentExpression(i, e) -> // e.g. i = 1
                let typeOfE = scanExpression e
                let typeOfI = symbolTable.GetIdentifierTypeSpec i
                checkCast typeOfE typeOfI
                typeOfI
               
            | Ast.ArrayVariableAssignmentExpression(i, e1, e2) -> // e.g. j[i] = 3
                checkArrayIndexType e1
                               
                let typeOfE2 = scanExpression e2
                let typeOfI = symbolTable.GetIdentifierTypeSpec i
                
                if not typeOfI.IsArray then
                   raise (cannotApplyIndexing (typeOfI.ToString()))
                
                if typeOfE2.IsArray then
                   raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))

                checkCast typeOfE2 typeOfI
               
                simpleType typeOfI.Type
               
            | Ast.BinaryExpression(e1, op, e2) -> // e.g. 1 + 2
                let typeOfE1 = scanExpression e1
                let typeOfE2 = scanExpression e2
                match op with
                | Ast.Or | Ast.And
                | Ast.Eq | Ast.NotEq
                | Ast.LtEq | Ast.Lt | Ast.GtEq | Ast.Gt ->
                    checkCast typeOfE2 typeOfE1
                    simpleType Ast.Bool
                | Ast.Sum | Ast.Diff | Ast.Mult | Ast.Div | Ast.Mod ->
                    typeOfE1
                    
            | Ast.UnaryExpression(op, e1) -> // e.g. not true
                let typeOfE = scanExpression e1
                match op with
                | Ast.Not -> 
                    checkCast typeOfE { Type = Ast.Bool; IsArray = false; }
                    simpleType Ast.Bool
                | Ast.Plus | Ast.Minus -> 
                    checkCast typeOfE { Type = Ast.Int; IsArray = false; }
                    simpleType Ast.Int
                    
            | Ast.FunctionCallExpression(i, a) -> // e.g. myFunc(1, "a")
                if not (functionTable.ContainsKey i) then
                    raise (nameDoesNotExist i)
                let calledFunction = functionTable.[i]
                let parameterTypes = calledFunction.ParameterTypes
                if List.length a <> List.length parameterTypes then
                    raise (wrongNumberOfArguments i (List.length parameterTypes) (List.length a))
                let argumentTypes = a |> List.map scanExpression
                List.iteri2 (checkArgument i) argumentTypes parameterTypes
                simpleType calledFunction.ReturnType
               
            | Ast.ArraySizeExpression(i) -> // e.g. myArray.size
                simpleType Ast.Int
               
            | Ast.IdentifierExpression(i) -> // e.g. myArray.size
                symbolTable.GetIdentifierTypeSpec i
                               
            | Ast.LiteralExpression(l) -> // e.g. 1
                match l with
                | Ast.BoolLiteral(b)    -> simpleType Ast.Bool
                | Ast.IntLiteral(i)     -> simpleType Ast.Int
                | Ast.DoubleLiteral(f)  -> simpleType Ast.Double
                | Ast.StringLiteral(f)  -> simpleType Ast.String
               
            | Ast.ArrayAllocationExpression(t, e) -> // e.g. new float[2]
                checkArrayIndexType e
                { Type = t; IsArray = true }

            | Ast.Empty -> simpleType Ast.Any
            | e -> raise (new Exception(sprintf "%A" e))
               
        self.Add(expression, expressionType)
        expressionType    
       
    and canConvertType fromType toType =
        match fromType, toType with
        | { Type = _; IsArray = false; }, { Type = Ast.Any; IsArray = false; } -> true
        | { Type = Ast.Int; IsArray = false; }, { Type = Ast.Double; IsArray = false; } -> true
        | f, t when f = t -> true
        | _ -> false

    and checkCast fromVariableType toVariableType =
        if not (canConvertType fromVariableType toVariableType) then 
            raise (cannotConvertType (fromVariableType.ToString()) (toVariableType.ToString()))

    and checkArgument identifier index fromArgumentType toParameterType =
        if not (canConvertType fromArgumentType toParameterType) then 
            raise (invalidArguments identifier (index + 1) (fromArgumentType.ToString()) (toParameterType.ToString()))
        
       
    do program |> List.iter scanDeclaration 