module MicroCosmo.Semantic

open System.Collections.Generic

open Errors

type SymbolScope(parent : SymbolScope option) =
    let mutable list = List.empty<Ast.VariableDeclarationStatement>

    let identifierFromDeclaration =
        function
        | (i, _, _) -> i

    let declaresIdentifier (identifierRef : Ast.IdentifierRef) declaration =
        (identifierFromDeclaration declaration) = identifierRef.Identifier

    member x.AddDeclaration (declaration : Ast.VariableDeclarationStatement)=
        if List.exists (fun x -> identifierFromDeclaration x = identifierFromDeclaration declaration) list then
            raise (variableAlreadyDefined (identifierFromDeclaration declaration))
        list <- declaration :: list

    member x.FindDeclaration identifierRef =
        let found = List.tryFind (fun x -> declaresIdentifier identifierRef x) list
        match found with
        | Some(d) -> d
        | None ->
            match parent with
            | Some(ss) -> ss.FindDeclaration identifierRef
            | None -> raise (nameDoesNotExist (identifierRef.Identifier))

type SymbolScopeStack() =
    let stack = new Stack<SymbolScope>()
    do stack.Push(new SymbolScope(None))

    member x.CurrentScope = stack.Peek()

    member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
    member x.Pop() = stack.Pop() |> ignore
    member x.AddDeclaration declaration =
        stack.Peek().AddDeclaration declaration

type VariableType =
    {
        Type    : Ast.TypeSpec;
    }
    override x.ToString() = x.Type.ToString()

let typeOfDeclaration (_, t, _) = { Type = t; }
let simpleType t = { Type = t; }

type SymbolTable(program) as self =
    inherit Dictionary<Ast.IdentifierRef, Ast.VariableDeclarationStatement>()

//    let sameIdentifierMap = Dictionary<string, string>()
//    let sameIdentifierCounters = Dictionary<string, int>()
    let whileStatementStack = Stack<Ast.WhileStatement>()
    let symbolScopeStack = new SymbolScopeStack()

    let rec scanDeclaration statement =
        match statement with
        | Ast.CommentStatement _ -> ()
        | Ast.VariableDeclarationStatement x -> symbolScopeStack.AddDeclaration x
        | Ast.FunctionDeclarationStatement x -> scanFunctionDeclaration x
        | other -> ()

    and scanFunctionDeclaration (_, parameters, functionReturnType, blockStatement, _) =
        let rec scanBlockStatement statements =
            symbolScopeStack.Push()
            statements |> List.iter scanStatement
            symbolScopeStack.Pop() |> ignore

        and scanStatement =
            function
            | Ast.VariableDeclarationStatement(i, t, g) ->
                symbolScopeStack.AddDeclaration (i, t, g)
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
            | Ast.CommentStatement _ -> ()
            | other -> ()

        and addIdentifierMapping identifierRef =
            let declaration = symbolScopeStack.CurrentScope.FindDeclaration identifierRef
            self.Add(identifierRef, declaration)

        and scanExpression (e : Ast.Expression) =
            match e with
            | Ast.VariableAssignmentExpression(i, e, _) ->
                addIdentifierMapping i
                scanExpression e
            | Ast.BinaryExpression(e1, _, e2, _) ->
                scanExpression e1
                scanExpression e2
            | Ast.UnaryExpression(_, e, _) ->
                scanExpression e
            | Ast.IdentifierExpression(i, _) ->
                addIdentifierMapping i
            | Ast.FunctionCallExpression(_, args, _) ->
                args |> List.iter scanExpression
            | Ast.LiteralExpression(l, _) -> ()
            | Ast.Empty -> ()

        let toBlockStatement = function Ast.BlockStatement x -> x

        symbolScopeStack.Push()
        parameters |> List.iter symbolScopeStack.AddDeclaration
        scanBlockStatement (toBlockStatement blockStatement)
        symbolScopeStack.Pop() |> ignore

    do program |> List.iter scanDeclaration

    member x.GetIdentifierTypeSpec identifierRef =
        typeOfDeclaration self.[identifierRef]

type FunctionTableEntry =
    {
        ReturnType     : Ast.TypeSpec;
        ParameterTypes : VariableType list;
    }

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

type ExpressionTypeTable(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Ast.Expression, VariableType>()

    let rec scanDeclaration =
        function
        | Ast.CommentStatement _ -> ()
        | Ast.FunctionDeclarationStatement(x) -> scanFunctionDeclaration x
        | other -> ()

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

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeTable;
    }

let analyze program =
    try
        let symbolTable   = SymbolTable(program)
        let functionTable = FunctionTable(program)

        if not (functionTable.ContainsKey "main") then
            raise (missingEntryPoint())

        let main = functionTable.["main"]
        if (main.ParameterTypes <> []) then
            raise (missingEntryPoint())

        let expressionTypes = new ExpressionTypeTable(program, functionTable, symbolTable)

        Result.Ok {
            SymbolTable     = symbolTable;
            ExpressionTypes = expressionTypes;
        }

    with _ as ex  -> Result.Error ex