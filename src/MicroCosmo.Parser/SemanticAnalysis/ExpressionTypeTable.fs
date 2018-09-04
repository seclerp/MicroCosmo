module MicroCosmo.ExpressionTypeTable

open System.Collections.Generic
open MicroCosmo
open MicroCosmo.SemanticAnalysis.SymbolTable
open MicroCosmo.SemanticAnalysis.FunctionTable
open MicroCosmo.SemanticAnalysis.VariableType
open MicroCosmo.CompilerErrors
open System

type ExpressionTypeTable(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Ast.Expression, VariableType>()
    
    let rec scanDeclaration =
        function
        | Ast.CommentStatement _ -> ()
        | Ast.FunctionDeclarationStatement(x) -> scanFunctionDeclaration x
        | Ast.VariableDeclarationStatement(_, t, e, _) -> 
            match e with
            | Some ex -> 
                let typeOfE = scanExpression ex
                let typeOfI = { Type = t; }
                checkCast typeOfE typeOfI
            | None -> ()
    
    and scanFunctionDeclaration (_, _, functionReturnType, blockStatement, _) =
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
            | Ast.VariableDeclarationStatement(_, t, e, _) -> 
                match e with
                | Some ex -> 
                    let typeOfE = scanExpression ex
                    let typeOfI = { Type = t; }
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
            | Ast.VariableAssignmentExpression(i, e, _) -> // e.g. i = 1
                let typeOfE = scanExpression e
                let typeOfI = symbolTable.GetIdentifierTypeSpec i
                checkCast typeOfE typeOfI
                typeOfI
               
            | Ast.BinaryExpression(e1, op, e2, _) -> // e.g. 1 + 2
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
                    
            | Ast.UnaryExpression(op, e1, _) -> // e.g. not true
                let typeOfE = scanExpression e1
                match op with
                | Ast.Not -> 
                    checkCast typeOfE { Type = Ast.Bool; }
                    simpleType Ast.Bool
                | Ast.Plus | Ast.Minus -> 
                    checkCast typeOfE { Type = Ast.Int; }
                    simpleType Ast.Int
                    
            | Ast.FunctionCallExpression(i, a, _) -> // e.g. myFunc(1, "a")
                if not (functionTable.ContainsKey i) then
                    raise (nameDoesNotExist i)
                let calledFunction = functionTable.[i]
                let parameterTypes = calledFunction.ParameterTypes
                if List.length a <> List.length parameterTypes then
                    raise (wrongNumberOfArguments i (List.length parameterTypes) (List.length a))
                let argumentTypes = a |> List.map scanExpression
                List.iteri2 (checkArgument i) argumentTypes parameterTypes
                simpleType calledFunction.ReturnType
               
            | Ast.IdentifierExpression(i, _) -> // e.g. myArray.size
                symbolTable.GetIdentifierTypeSpec i
                               
            | Ast.LiteralExpression(l, _) -> // e.g. 1
                match l with
                | Ast.BoolLiteral(b)    -> simpleType Ast.Bool
                | Ast.IntLiteral(i)     -> simpleType Ast.Int
                | Ast.DoubleLiteral(f)  -> simpleType Ast.Double
                | Ast.StringLiteral(f)  -> simpleType Ast.String
               
            | Ast.Empty -> simpleType Ast.NoneType
               
        self.Add(expression, expressionType)
        expressionType    
       
    and canConvertType fromType toType =
        match fromType, toType with
        | { Type = Ast.Int; }, { Type = Ast.Double; } -> true
        | _, { Type = Ast.String; } -> true
        | f, t when f = t -> true
        | _ -> false

    and checkCast fromVariableType toVariableType =
        if not (canConvertType fromVariableType toVariableType) then 
            raise (cannotConvertType (fromVariableType.ToString()) (toVariableType.ToString()))

    and checkArgument identifier index fromArgumentType toParameterType =
        if not (canConvertType fromArgumentType toParameterType) then 
            raise (invalidArguments identifier (index + 1) (fromArgumentType.ToString()) (toParameterType.ToString()))
        
       
    do program |> List.iter scanDeclaration 