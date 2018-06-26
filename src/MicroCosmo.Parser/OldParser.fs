module MicroCosmo.OldParser
//
//open Piglet.Parser.Configuration
//open Piglet.Parser
//open MicroCosmo.Ast
//open MicroCosmo.ParsingUtilities
//open MicroCosmo.CompilerErrors
//
//let createParser () =
//    // Piglet configurator
//    let configurator = ParserFactory.Configure<obj>()
//    
//    // Non terminal configuration
//    let nonTerminal () = new NonTerminalWrapper<'a>(configurator.CreateNonTerminal())
//    
//    let program                   = nonTerminal<Program>()
//    let declarationList           = nonTerminal<Declaration list>()
//    let declaration               = nonTerminal<Declaration>()
//    let localVariableDeclaration  = nonTerminal<VariableDeclaration>()
//    let functionDeclaration       = nonTerminal<FunctionDeclaration>()
//    let typeSpec                  = nonTerminal<TypeSpec>()
//    let parameters                = nonTerminal<Parameters>()
//    let parameterList             = nonTerminal<Parameters>()
//    let parameter                 = nonTerminal<VariableDeclaration>()
//    let optionalStatementList     = nonTerminal<Statement list>()
//    let statementList             = nonTerminal<Statement list>()
//    let statement                 = nonTerminal<Statement>()
//    let expressionStatement       = nonTerminal<ExpressionStatement>()
//    let whileStatement            = nonTerminal<WhileStatement>()
//    let blockStatement            = nonTerminal<BlockStatement>()
//    let optionalLocalDeclarations = nonTerminal<VariableDeclaration list>()
//    let localDeclarations         = nonTerminal<VariableDeclaration list>()
//    let localDeclaration          = nonTerminal<VariableDeclaration>()
//    let ifStatement               = nonTerminal<IfStatement>()
//    let optionalElseStatement     = nonTerminal<Statement option>()
//    let returnStatement           = nonTerminal<Expression option>()
//    let breakStatement            = nonTerminal<unit>()
//    let expression                = nonTerminal<Expression>()
//    let unaryOperator             = nonTerminal<UnaryOperator>()
//    let optionalArguments         = nonTerminal<Arguments>()
//    let arguments                 = nonTerminal<Arguments>()
//    
//    // Terminal configuration
//    let terminalParse regex onParse =
//        new TerminalWrapper<'a>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))
//    
//    let terminal regex =
//        new TerminalWrapper<string>(configurator.CreateTerminal(regex))
//        
//    // Keywords
//    let ifKeyword        = terminal      "if"
//    let elseKeyword      = terminal      "else"
//    let whileKeyword     = terminal      "while"
//    let returnKeyword    = terminal      "return"
//    let breakKeyword     = terminal      "break"
//    let lengthKeyword    = terminal      "length"
//    let arrayKeyword     = terminal      "array"
//    
//    // Type names
//    let noneKeyword      = terminalParse "none"                     (fun s -> Ast.None)
//    let anyKeyword       = terminalParse "any"                      (fun s -> Ast.Any)
//    let stringKeyword    = terminalParse "string"                   (fun s -> Ast.String)
//    let boolKeyword      = terminalParse "bool"                     (fun s -> Ast.Bool)
//    let intKeyword       = terminalParse "int"                      (fun s -> Ast.Int)
//    let doubleKeyword    = terminalParse "double"                   (fun s -> Ast.Double)
//        
//    // Literals
//    let stringLiteral    = terminalParse @"\""(([^\""]|\\\"")*[^\\])?\""|\'(([^\""]|\\\"")*[^\\])?\'"  (fun s -> Ast.StringLiteral((s.Substring(1, s.Length - 2))))
//    let intLiteral       = terminalParse @"\d+"                     (fun s -> Ast.IntLiteral)
//    let doubleLiteral    = terminalParse @"\d*\.\d+"                (fun s -> Ast.DoubleLiteral)
//    let trueLiteral      = terminalParse "true"                     (fun s -> Ast.BoolLiteral(true))
//    let falseLiteral     = terminalParse "false"                    (fun s -> Ast.BoolLiteral(false))
//
//    // Operators
//    let plus             = terminal      @"\+"
//    let minus            = terminal      "-"
//    let plusPlus         = terminal      @"\+\+"
//    let minusMinus       = terminal      "--"
//    let not              = terminal      "not"
//    let asterisk         = terminal      @"\*"
//    let doubleAsterisk   = terminal      @"\*\*"
//    let percent          = terminal      "%"
//    let forwardSlash     = terminal      "/"
//    let singleEquals     = terminal      "="
//    let or_              = terminal      "or"
//    let and_             = terminal      "and"
//    let is               = terminal      "is"
//    let isNot            = terminal      "is not"
//    let openAngleEquals  = terminal      "<="
//    let openAngle        = terminal      "<"
//    let closeAngleEquals = terminal      ">="
//    let closeAngle       = terminal      ">"
//    
//    // Common
//    let identifier       = terminalParse "[a-zA-Z_\$][a-zA-Z_\$0-9]*"   (fun s -> s)
//    let dot           = terminal      @"\."
//    let openParen        = terminal      @"\("
//    let closeParen       = terminal      @"\)"
//    let openCurly        = terminal      @"\{"
//    let closeCurly       = terminal      @"\}"
//    let openSquare       = terminal      @"\["
//    let closeSquare      = terminal      @"\]"
//    let colon            = terminal      ":"
//    let comma            = terminal      ","
//
//    // Precedence
//    let optionalElsePrecedenceGroup = configurator.LeftAssociative()
//    
//    configurator.LeftAssociative(downcast elseKeyword.Symbol) |> ignore
//    configurator.LeftAssociative(downcast singleEquals.Symbol) |> ignore
//    configurator.LeftAssociative(downcast or_.Symbol) |> ignore
//    
//    configurator.LeftAssociative(
//        downcast is.Symbol, 
//        downcast isNot.Symbol
//    ) |> ignore
//                                 
//    configurator.LeftAssociative(
//        downcast openAngleEquals.Symbol, 
//        downcast openAngle.Symbol, 
//        downcast closeAngleEquals.Symbol, 
//        downcast closeAngle.Symbol
//    ) |> ignore
//                                 
//    configurator.LeftAssociative(
//        downcast and_.Symbol
//    ) |> ignore
//    
//    configurator.LeftAssociative(
//        downcast not.Symbol, 
//        downcast plus.Symbol, 
//        downcast minus.Symbol
//    ) |> ignore
//                                 
//    configurator.LeftAssociative(
//        downcast asterisk.Symbol, 
//        downcast forwardSlash.Symbol, 
//        downcast percent.Symbol
//    ) |> ignore
//                                 
//    configurator.LeftAssociative(
//        downcast doubleAsterisk.Symbol
//    ) |> ignore
//    
//    let binaryExpressionPrecedenceGroup = configurator.LeftAssociative()
//    let unaryExpressionPrecedenceGroup  = configurator.RightAssociative()
//    
//    // Productions
//    program.AddProduction(declarationList).SetReduceToFirst()
//    
//    declarationList.AddProduction(declarationList, declaration)
//        .SetReduceFunction (fun a b -> a @ [b])
//    declarationList.AddProduction(declaration)
//        .SetReduceFunction (fun a -> [a])
//    
//    declaration.AddProduction(localVariableDeclaration)
//        .SetReduceFunction (fun a -> Ast.LocalVariableDeclaration a)
//    declaration.AddProduction(functionDeclaration)
//        .SetReduceFunction (fun a -> Ast.FunctionDeclaration a)
//    
//    localVariableDeclaration.AddProduction(identifier, colon, typeSpec)
//        .SetReduceFunction (fun a _ b -> Ast.ScalarVariableDeclaration(a, b))
//    localVariableDeclaration.AddProduction(identifier, colon, typeSpec, arrayKeyword)
//        .SetReduceFunction (fun a _ b _ -> Ast.ArrayVariableDeclaration(a, b))    
//    
//    typeSpec.AddProduction(noneKeyword).SetReduceFunction (fun _ -> Ast.None)
//    typeSpec.AddProduction(anyKeyword).SetReduceToFirst()
//    typeSpec.AddProduction(stringKeyword).SetReduceToFirst()
//    typeSpec.AddProduction(intKeyword).SetReduceToFirst()
//    typeSpec.AddProduction(doubleKeyword).SetReduceToFirst()
//    typeSpec.AddProduction(boolKeyword).SetReduceToFirst()
//    
//    functionDeclaration.AddProduction(identifier, openParen, parameters, closeParen, colon, typeSpec, blockStatement)
//        .SetReduceFunction (fun a _ c _ _ f g -> (a, c, f, g))
//    functionDeclaration.AddProduction(identifier, openParen, parameters, closeParen, blockStatement)
//        .SetReduceFunction (fun a _ c _ e -> (a, c, Ast.None, e))   
//        
//    parameters.AddProduction(parameterList).SetReduceToFirst()
//    
//    parameterList.AddProduction(parameterList, comma, parameter)
//        .SetReduceFunction (fun a _ c -> a @ [c])
//    parameterList.AddProduction(parameter)
//        .SetReduceFunction (fun a -> [a])
//        
//    parameter.AddProduction(identifier, colon, typeSpec)
//        .SetReduceFunction (fun a _ c -> Ast.ScalarVariableDeclaration(a, c))
//    parameter.AddProduction(identifier, colon, typeSpec, arrayKeyword)
//        .SetReduceFunction (fun a _ c _ -> Ast.ArrayVariableDeclaration(a, c))
//    
//    statementList.AddProduction(statementList, statement)
//        .SetReduceFunction (fun a b -> a @ [b])
//    statementList.AddProduction(statement)
//        .SetReduceFunction (fun a -> [a])
//        
//    statement.AddProduction(expressionStatement)
//        .SetReduceFunction (fun a -> Ast.ExpressionStatement a)
//    statement.AddProduction(blockStatement)  
//        .SetReduceFunction (fun a -> Ast.BlockStatement a)
//    statement.AddProduction(ifStatement)        
//        .SetReduceFunction (fun a -> Ast.IfStatement a)
//    statement.AddProduction(whileStatement)     
//        .SetReduceFunction (fun a -> Ast.WhileStatement a)
//    statement.AddProduction(returnStatement)    
//        .SetReduceFunction (fun a -> Ast.ReturnStatement a)
//    statement.AddProduction(breakStatement)     
//        .SetReduceFunction (fun a -> Ast.BreakStatement)
//    
//    expressionStatement.AddProduction(expression)
//        .SetReduceFunction (fun a -> Ast.Expression a)
//        
//    whileStatement.AddProduction(whileKeyword, openParen, expression, closeParen, statement)
//        .SetReduceFunction (fun a b c d e -> (c, e))
//    
//    blockStatement.AddProduction(openCurly, optionalLocalDeclarations, optionalStatementList, closeCurly)
//        .SetReduceFunction (fun _ b c _ -> (b, c))
//    
//    optionalLocalDeclarations.AddProduction(localDeclarations)
//        .SetReduceToFirst()
//    optionalLocalDeclarations.AddProduction()                 
//        .SetReduceFunction (fun () -> [])
//    
//    localDeclarations.AddProduction(localDeclarations, localDeclaration)
//        .SetReduceFunction (fun a b -> a @ [b])
//    localDeclarations.AddProduction(localDeclaration)                   
//        .SetReduceFunction (fun a -> [a])
//    
//    localDeclaration.AddProduction(identifier, colon, typeSpec)                         
//        .SetReduceFunction (fun a _ c -> Ast.ScalarVariableDeclaration(a, c))
//    localDeclaration.AddProduction(identifier, colon, typeSpec, arrayKeyword)
//        .SetReduceFunction (fun a _ c _ -> Ast.ArrayVariableDeclaration(a, c))
//    
//    ifStatement.AddProduction(ifKeyword, openParen, expression, closeParen, statement, optionalElseStatement)
//        .SetReduceFunction (fun _ _ c _ e f -> (c, e, f))
//    
//    let elseStatementProduction = optionalElseStatement.AddProduction(elseKeyword, statement)
//    elseStatementProduction.SetReduceFunction (fun _ b -> Option.Some b)
//    elseStatementProduction.SetPrecedence optionalElsePrecedenceGroup
//    
//    let elseEpsilonProduction = optionalElseStatement.AddProduction()
//    elseEpsilonProduction.SetReduceFunction (fun () -> Option.None)
//    elseEpsilonProduction.SetPrecedence optionalElsePrecedenceGroup
//    
//    returnStatement.AddProduction(returnKeyword, expression)
//        .SetReduceFunction (fun _ b -> Option.Some b)
//    returnStatement.AddProduction(returnKeyword)            
//        .SetReduceFunction (fun _ -> Option.None)
//    
//    breakStatement.AddProduction(breakKeyword)
//        .SetReduceFunction (fun _ -> ())
//    
//    expression.AddProduction(identifier, singleEquals, expression)
//        .SetReduceFunction (fun a _ c -> Ast.VariableAssignmentExpression({ Identifier = a }, c))
//    expression.AddProduction(identifier, openSquare, expression, closeSquare, singleEquals, expression)
//        .SetReduceFunction (fun a _ c _ _ f -> Ast.ArrayVariableAssignmentExpression({ Identifier = a }, c, f))
//    
//    expression.AddProduction(expression, or_, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Or, c))
//    expression.AddProduction(expression, is, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Eq, c))
//    expression.AddProduction(expression, isNot, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.NotEq, c))
//    expression.AddProduction(expression, openAngleEquals, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.LtEq, c))
//    expression.AddProduction(expression, openAngle, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Lt, c))
//    expression.AddProduction(expression, closeAngleEquals, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.GtEq, c))
//    expression.AddProduction(expression, closeAngle, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Gt, c))
//    expression.AddProduction(expression, and_, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.And, c))
//    expression.AddProduction(expression, plus, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Sum, c))
//    expression.AddProduction(expression, minus, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Diff, c))
//    expression.AddProduction(expression, asterisk, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Mult, c))
//    expression.AddProduction(expression, forwardSlash, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Div, c))
//    expression.AddProduction(expression, percent, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Mod, c))
//    expression.AddProduction(expression, doubleAsterisk, expression)
//        .SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Pow, c))
//    
//    let unaryExpressionProduction = expression.AddProduction(unaryOperator, expression)
//    unaryExpressionProduction.SetReduceFunction (fun a b -> Ast.UnaryExpression(a, b))
//    unaryExpressionProduction.SetPrecedence unaryExpressionPrecedenceGroup
//    
//    expression.AddProduction(openParen, expression, closeParen).SetReduceFunction (fun _ b _ -> b)
//    
//    expression.AddProduction(identifier)
//        .SetReduceFunction (fun a -> Ast.IdentifierExpression { Identifier = a })
//    expression.AddProduction(identifier, openSquare, expression, closeSquare)
//        .SetReduceFunction (fun a _ c _ -> Ast.ArrayIdentifierExpression({ Identifier = a }, c))
//    expression.AddProduction(identifier, openParen, optionalArguments, closeParen)
//        .SetReduceFunction (fun a _ c _ -> Ast.FunctionCallExpression(a, c))
//    expression.AddProduction(identifier, dot, lengthKeyword)
//        .SetReduceFunction (fun a _ _ -> Ast.ArraySizeExpression { Identifier = a })
//    
//    expression.AddProduction(stringLiteral)
//        .SetReduceFunction (fun a -> Ast.LiteralExpression a)
//    expression.AddProduction(trueLiteral) 
//        .SetReduceFunction (fun a -> Ast.LiteralExpression a)
//    expression.AddProduction(falseLiteral)
//        .SetReduceFunction (fun a -> Ast.LiteralExpression a)
//    expression.AddProduction(intLiteral)  
//        .SetReduceFunction (fun a -> Ast.LiteralExpression a)
//    expression.AddProduction(doubleLiteral)
//        .SetReduceFunction (fun a -> Ast.LiteralExpression a)
//    
//    expression.AddProduction(typeSpec, arrayKeyword, openSquare, expression, closeSquare)
//        .SetReduceFunction (fun a _ _ d _ -> Ast.ArrayAllocationExpression(a, d))
//    
//    unaryOperator.AddProduction(not)
//        .SetReduceFunction (fun a -> Ast.Not)
//    unaryOperator.AddProduction(minus)      
//        .SetReduceFunction (fun a -> Ast.Minus)
//    unaryOperator.AddProduction(plus)       
//        .SetReduceFunction (fun a -> Ast.Plus)
//    
//    optionalArguments.AddProduction(arguments)
//        .SetReduceToFirst()
//    optionalArguments.AddProduction()         
//        .SetReduceFunction (fun () -> [])
//    
//    arguments.AddProduction(arguments, comma, expression)
//        .SetReduceFunction (fun a _ c -> a @ [c])
//    arguments.AddProduction(expression)                  
//        .SetReduceFunction (fun a -> [a])
//    
//    // Ignore whitespace, comments and semicolon
//    configurator.LexerSettings.Ignore <- [| @"\s+"; @"/\*[^(\*/)]*\*/"; @"\#[^\n]*\n"; ";" |]
//    
//    // Result parser
//    configurator.CreateParser()
//
//let parse (input : string) =
//    let parser = createParser ()
//    
//    try
//        parser.Parse(input) :?> Program
//    with
//        | :? Piglet.Lexer.LexerException as ex ->
//            raise (lexerError ex.Message)
//        | :? Piglet.Parser.ParseException as ex ->
//            raise (parserError ex.Message)
//    