module MicroCosmo.ExpressionTypeTable

open System.Collections.Generic
open MicroCosmo
open MicroCosmo.SymbolTable
open MicroCosmo.FunctionTable
open MicroCosmo.VariableType
open MicroCosmo.CompilerErrors

type ExpressionTypeTable(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Ast.Expression, VariableType>(HashIdentity.Reference)
    
    let rec scanDeclaration =
        function
        | Ast.FunctionDeclaration(x) -> scanFunctionDeclaration x
        | _ -> ()
    
    and scanFunctionDeclaration (_, _, functionReturnType, compoundStatement) =
        let rec scanBlockStatement (_, statements) =
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
                if typeOfE <> simpleType functionReturnType then
                    raise (cannotConvertType (typeOfE.ToString()) (functionReturnType.ToString()))
            | _ -> () 
            
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
                    if typeOfE <> typeOfI then raise (cannotConvertType (typeOfE.ToString()) (typeOfI.ToString()))
                    typeOfI
                    
                | Ast.ArrayVariableAssignmentExpression(i, e1, e2) -> // e.g. j[i] = 3
                    checkArrayIndexType e1
                    
                    let typeOfE2 = scanExpression e2
                    let typeOfI = symbolTable.GetIdentifierTypeSpec i
                    
                    if not typeOfI.IsArray then
                        raise (cannotApplyIndexing (typeOfI.ToString()))
                    
                    if typeOfE2.IsArray then
                        raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))
                    
                    if typeOfE2.Type <> typeOfI.Type then 
                        raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))
                    
                    simpleType typeOfI.Type
                    
                | Ast.BinaryExpression(e1, op, e2) -> // e.g. 1 + 2
                    let typeOfE1 = scanExpression e1
                    let typeOfE2 = scanExpression e2
                    match op with
                    | Ast.Or | Ast.And ->
                        match typeOfE1, typeOfE2 with
                        | { Type = Bool; IsArray = false; }, { Type = Ast.Bool; IsArray = false; } -> ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        simpleType Ast.Bool
                    | Ast.Eq | Ast.NotEq ->
                        match typeOfE1, typeOfE2 with
                        | { Type = a; IsArray = false; }, { Type = b; IsArray = false; } when a = b && a <> Ast.NoneType -> ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        simpleType Ast.Bool
                    | Ast.LtEq | Ast.Lt | Ast.GtEq | Ast.Gt ->
                        match typeOfE1, typeOfE2 with
                        | { Type = Ast.Int; IsArray = false; }, { Type = Ast.Int; IsArray = false; }
                        | { Type = Ast.Double; IsArray = false; }, { Type = Ast.Double; IsArray = false; } ->
                            ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        simpleType Ast.Bool
                    | Ast.Sum | Ast.Diff | Ast.Mult | Ast.Div | Ast.Mod ->
                        typeOfE1
                        
                | Ast.FunctionCallExpression(i, a) -> // e.g. myFunc(1, "a")
                    if not (functionTable.ContainsKey i) then
                        raise (nameDoesNotExist i)
                    let calledFunction = functionTable.[i]
                    let parameterTypes = calledFunction.ParameterTypes
                    if List.length a <> List.length parameterTypes then
                        raise (wrongNumberOfArguments i (List.length parameterTypes) (List.length a))
                    let argumentTypes = a |> List.map scanExpression
                    let checkTypesMatch index l r =
                        if l <> r then raise (invalidArguments i (index + 1) (l.ToString()) (r.ToString()))
                    List.iteri2 checkTypesMatch argumentTypes parameterTypes
                    simpleType calledFunction.ReturnType
                    
                | Ast.ArraySizeExpression(i) -> // e.g. myArray.size
                    simpleType Ast.Int
                    
                | Ast.LiteralExpression(l) -> // e.g. 1
                    match l with
                    | Ast.BoolLiteral(b)    -> simpleType Ast.Bool
                    | Ast.IntLiteral(i)     -> simpleType Ast.Int
                    | Ast.DoubleLiteral(f)  -> simpleType Ast.Double
                    
                | Ast.ArrayAllocationExpression(t, e) -> // e.g. new float[2]
                    checkArrayIndexType e
                    { Type = t; IsArray = true }
                    
            self.Add(expression, expressionType)
            expressionType 
            
        do program |> List.iter scanDeclaration 