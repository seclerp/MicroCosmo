module MicroCosmo.IR.ILBuilder

open System.Collections.Generic
open MicroCosmo.SemanticAnalysis
open MicroCosmo.SemanticAnalysis.SemanticAnalysisResult
open MicroCosmo.IL
open MicroCosmo

type ILVariableScope =
    | FieldScope of ILVariable
    | ArgumentScope of int16
    | LocalScope of int16

type VariableMappingDictionary() = 
    inherit Dictionary<Ast.VariableDeclarationStatement, ILVariableScope>()

module private ILBuilderUtilities =
    open System

    let stringToType =
        function
        | "none"   -> Ast.NoneType
        | "bool"   -> Ast.Bool
        | "int"    -> Ast.Int
        | "double" -> Ast.Double
        | "string" -> Ast.String

    let typeOf =
        function
        | Ast.NoneType  -> typeof<Void>
        | Ast.Bool      -> typeof<bool>
        | Ast.Int       -> typeof<int>
        | Ast.Double    -> typeof<float>
        | Ast.String    -> typeof<string>

    let createILVariable (i, t, _, _) =
        {
            ILVariable.Type = typeOf t; 
            Name = i;
        }

open ILBuilderUtilities
open MicroCosmo.Ast

type ILMethodBuilder(semanticAnalysisResult : SemanticAnalysisResult,
                     variableMappings : VariableMappingDictionary) =
    let mutable argumentIndex = 0s
    let mutable localIndex = 0s
    let arrayAssignmentLocals = Dictionary<Ast.Expression, int16>()
    let mutable labelIndex = 0
    let currentWhileStatementEndLabel = Stack<ILLabel>()

    let lookupILVariableScope identifierRef =
        let a = Seq.head variableMappings.Keys
        let b = semanticAnalysisResult.SymbolTable.[identifierRef]
        let c = LanguagePrimitives.PhysicalEquality a b
        variableMappings.[semanticAnalysisResult.SymbolTable.[identifierRef]]

    let makeLabel() =
        let result = labelIndex
        labelIndex <- labelIndex + 1
        result

    let rec processBinaryExpression =
        function
        | (l, Ast.Or, r) ->
            let leftIsFalseLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression l
                          [ ILOpCode.Brfalse leftIsFalseLabel ]
                          [ ILOpCode.Ldc_I4 1 ]
                          [ ILOpCode.Br endLabel ]
                          [ ILOpCode.Label leftIsFalseLabel ]
                          processExpression r
                          [ ILOpCode.Label endLabel ] ]
        | (l, Ast.And, r) ->
            let leftIsTrueLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression l
                          [ ILOpCode.Brtrue leftIsTrueLabel ]
                          [ ILOpCode.Ldc_I4 0 ]
                          [ ILOpCode.Br endLabel ]
                          [ ILOpCode.Label leftIsTrueLabel ]
                          processExpression r
                          [ ILOpCode.Label endLabel ] ]
        | (l, op, r) -> 
            let leftProcessed = processExpression l
            let rightProcessed = processExpression r
            
            match op with
            | Ast.Sum -> 
                let leftType = semanticAnalysisResult.ExpressionTypes.[l].Type
                let rightType = semanticAnalysisResult.ExpressionTypes.[r].Type
                
                match (leftType, rightType) with
                | (Ast.String, Ast.String) -> List.concat [ leftProcessed;
                                                            rightProcessed;
                                                            [ processStringConcatOperator ] ]
                | _ -> List.concat [ leftProcessed;
                                     rightProcessed;
                                     [ processBinaryOperator op ] ]
            | Ast.To -> processExplicitCastOperator l r
                 
            | _ -> 
                List.concat [ leftProcessed;
                              rightProcessed;
                              [ processBinaryOperator op ] ]
    
    and processExplicitCastOperator exprFrom exprTo =
        let toExprToType = function Ast.IdentifierExpression ({Identifier = i}, _) -> stringToType i
        let toType = toExprToType exprTo
        match toType with
        | Ast.String -> 
            List.concat [ processExpression exprFrom ;
                          [ ILOpCode.CallClr((typeOf toType).GetMethod("ToString", Array.empty)) ]; ]
        | Ast.Int ->
            List.concat [ processExpression exprFrom ;
                          [ ILOpCode.Conv_i4 ] ]
        | Ast.Double ->
            List.concat [ processExpression exprFrom ;
                          [ ILOpCode.Conv_r8 ] ]
    
    and processStringConcatOperator =
        ILOpCode.CallClr(typeof<System.String>.GetMethod("Concat", [| typeof<System.String>; typeof<System.String> |]))

    and processBinaryOperator =
        function
        | Ast.Sum   -> ILOpCode.Add
        | Ast.Div   -> ILOpCode.Div
        | Ast.Mult  -> ILOpCode.Mul
        | Ast.Mod   -> ILOpCode.Rem
        | Ast.Diff  -> ILOpCode.Sub
        | Ast.Eq    -> ILOpCode.Ceq
        | Ast.Gt    -> ILOpCode.Cgt
        | Ast.GtEq  -> ILOpCode.Cge
        | Ast.Lt    -> ILOpCode.Clt
        | Ast.LtEq  -> ILOpCode.Cle
        | o         -> failwith (sprintf "Unsupported binary operator: %A" o)

    and processIdentifierLoad identifierRef =
        match lookupILVariableScope identifierRef with
        | ILVariableScope.FieldScope(v)    -> [ ILOpCode.Ldsfld v ]
        | ILVariableScope.ArgumentScope(i) -> [ ILOpCode.Ldarg i ]
        | ILVariableScope.LocalScope(i)    -> [ ILOpCode.Ldloc i ]

    and processIdentifierStore identifierRef =
        match lookupILVariableScope identifierRef with
        | ILVariableScope.FieldScope(v)    -> [ ILOpCode.Stsfld v ]
        | ILVariableScope.ArgumentScope(i) -> [ ILOpCode.Starg i ]
        | ILVariableScope.LocalScope(i)    -> [ ILOpCode.Stloc i ]

    and processExpression expression =
        match expression with
        | Ast.VariableAssignmentExpression(i, e, _) ->
            List.concat [ processExpression e
                          [ ILOpCode.Dup ]
                          processIdentifierStore i ]
        | Ast.BinaryExpression(a, b, c, _) -> processBinaryExpression (a, b, c)
        | Ast.UnaryExpression(op, e, _) ->
            List.concat [ processExpression e
                          processUnaryOperator op]
        | Ast.IdentifierExpression(i, _) -> processIdentifierLoad i
         | Ast.FunctionCallExpression(i, a, _) ->
            List.concat [ a |> List.collect processExpression
                          [ ILOpCode.Call i ] ]
        | Ast.LiteralExpression(l, _) ->
            match l with
            | Ast.IntLiteral(x)     -> [ ILOpCode.Ldc_I4 x ]
            | Ast.DoubleLiteral(x)  -> [ ILOpCode.Ldc_R8 x ]
            | Ast.BoolLiteral(x)    -> [ (if x then ILOpCode.Ldc_I4(1) else ILOpCode.Ldc_I4 0) ]
            | Ast.StringLiteral(x)  -> [ ILOpCode.Ldstr x ]

    and processUnaryOperator =
        function
        | Ast.Not   -> [ ILOpCode.Ldc_I4 0; ILOpCode.Ceq ]
        | Ast.Minus -> [ ILOpCode.Neg ]
        | Ast.Plus  -> [ ]

    and processStatement =
        function
        | Ast.ExpressionStatement(x) ->
            match x with
            | Ast.Empty -> []
            | x ->
                let isNotVoid = semanticAnalysisResult.ExpressionTypes.[x].Type <> Ast.NoneType
                List.concat [ processExpression x
                              (if isNotVoid then [ ILOpCode.Pop ] else []) ]
                
        | Ast.BlockStatement(s) -> s |> List.collect processStatement
        | Ast.IfStatement(e, s1, Some(s2)) ->
            let thenLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression e
                          [ ILOpCode.Brtrue thenLabel ]
                          processStatement s2
                          [ ILOpCode.Br endLabel ]
                          [ ILOpCode.Label thenLabel ]
                          processStatement s1
                          [ ILOpCode.Label endLabel ] ]
        | Ast.IfStatement(e, s1, None) ->
            let thenLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression e
                          [ ILOpCode.Brtrue thenLabel ]
                          [ ILOpCode.Br endLabel ]
                          [ ILOpCode.Label thenLabel ]
                          processStatement s1
                          [ ILOpCode.Label endLabel ] ]
        | Ast.WhileStatement(e, s) ->
            let startLabel = makeLabel()
            let conditionLabel = makeLabel()
            let endLabel = makeLabel()
            currentWhileStatementEndLabel.Push endLabel
            let result = List.concat [ [ ILOpCode.Br conditionLabel ]
                                       [ ILOpCode.Label startLabel ]
                                       processStatement s
                                       [ ILOpCode.Label conditionLabel ]
                                       processExpression e
                                       [ ILOpCode.Brtrue startLabel ]
                                       [ ILOpCode.Label endLabel ] ]
            currentWhileStatementEndLabel.Pop() |> ignore
            result
        | Ast.ReturnStatement(x) ->
            match x with
            | Some(x) -> (processExpression x) @ [ ILOpCode.Ret ]
            | None    -> [ ILOpCode.Ret ]
        | Ast.BreakStatement -> [ ILOpCode.Br (currentWhileStatementEndLabel.Peek()) ]
        | _ -> []

    let processVariableDeclaration (mutableIndex : byref<_>) f (d : Ast.VariableDeclarationStatement) =
        let v = createILVariable d
        variableMappings.Add(d, f mutableIndex)
        mutableIndex <- mutableIndex + 1s
        v

    let processLocalDeclaration (declaration : Ast.VariableDeclarationStatement) =
        processVariableDeclaration &localIndex (fun i -> LocalScope i) declaration
        
    let processParameter (declaration : Ast.VariableDeclarationStatement) =
        processVariableDeclaration &argumentIndex (fun i -> ArgumentScope i) declaration

    let rec collectLocalDeclarations statement =
        let rec fromStatement =
            function
            | Ast.ExpressionStatement(es) ->
                match es with
                | Ast.Empty -> []
                | e -> fromExpression e
                
            | Ast.BlockStatement(statements) ->
                List.concat [ statements |> List.collect collectLocalDeclarations ]
                
            | Ast.IfStatement(e, s1, Some(s2)) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s1
                              collectLocalDeclarations s2 ]
                              
            | Ast.IfStatement(e, s1, None) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s1 ]
                              
            | Ast.WhileStatement(e, s) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s ]
                              
            | Ast.ReturnStatement(Some(e)) ->
                List.concat [ fromExpression e ]
                
            | Ast.VariableDeclarationStatement decl -> 
                [ processLocalDeclaration decl ]
                
            | _ -> []

        and fromExpression =
            function
            | Ast.VariableAssignmentExpression(i, e, _) -> fromExpression e
            | Ast.BinaryExpression(l, op, r, _)      -> List.concat [ fromExpression l; fromExpression r; ]
            | Ast.UnaryExpression(op, e, _)          -> fromExpression e
            | Ast.FunctionCallExpression(i, a, _)    -> a |> List.collect fromExpression
            | _ -> []

        fromStatement statement

    member x.BuildMethod(name, parameters, returnType, blockStatement : Ast.Statement, _) =
        let statements = match blockStatement with Ast.BlockStatement stmnts -> stmnts
    
        {
            Name       = name;
            ReturnType = typeOf returnType;
            Parameters = parameters |> List.map processParameter;
            Locals     = statements |> List.collect collectLocalDeclarations;
            Body       = statements |> List.collect processStatement;
        }

type ILBuilder(semanticAnalysisResult) =
    let variableMappings = new VariableMappingDictionary()

    let processStaticVariableDeclaration (d : Ast.VariableDeclarationStatement) =
        let v = createILVariable d
        variableMappings.Add(d, ILVariableScope.FieldScope(v))
        v

    member x.BuildClass (program : Ast.Program) =
        let variableDeclarations =
            program
            |> List.choose (fun x ->
                match x with
                | Ast.VariableDeclarationStatement x -> Some x
                | _ -> None)
    
        let functionDeclarations =
            program
            |> List.choose (fun x ->
                match x with
                | Ast.FunctionDeclarationStatement a -> Some a
                | _ -> None)

        let processFunctionDeclaration (functionDeclaration : Ast.FunctionDeclarationStatement)=
            let ilMethodBuilder = new ILMethodBuilder(semanticAnalysisResult, variableMappings)
            ilMethodBuilder.BuildMethod functionDeclaration

        let builtInMethods = [
            {
                Name = "readstr";
                ReturnType = typeof<string>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         Ret ];
            };
            {
                Name = "readint";
                ReturnType = typeof<int>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToInt32", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "readreal";
                ReturnType = typeof<float>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToDouble", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "println";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<System.Object>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.Object> |]))
                         Ret ];
            };
            {
                Name = "print";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<obj>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)
                         CallClr(typeof<System.Console>.GetMethod("Write", [| typeof<System.Object> |]))
                         Ret ];
            };
        ]

        {
            Fields  = variableDeclarations |> List.map (fun x -> processStaticVariableDeclaration x);
            Methods = List.concat [ builtInMethods
                                    functionDeclarations |> List.map processFunctionDeclaration ];
        }