module MicroCosmo.Terminants

// Keywords
let LET             = "let"
let FUNC            = "func"
let IF              = "if"
let ELSE            = "else"
let WHILE           = "while"
let RETURN          = "return"
let BREAK           = "break"

// Type names
let NONE            = "none"
let STRING          = "string"
let BOOL            = "bool"
let INT             = "int"
let DOUBLE          = "double"

// Comments
let COMMENT_START  = '#' 

// Literals
let STRING_LIT      = @"\""(([^\""]|\\\"")*[^\\])?\""|\'(([^\""]|\\\"")*[^\\])?\'"
let INT_LIT         = @"\d+"
let DOUBLE_LIT      = @"\d*\.\d+" 
let TRUE_LIT        = "true" 
let FALSE_LIT       = "false" 

// Operators
let PLUS            = "+"
let MINUS           = "-"
let NOT             = "not"
let ASTERISK        = "*"
let DOUBLEASTERISK  = "**"
let PERCENT         = "%"
let FORWARDSLASH    = "/"
let SINGLEEQUALS    = "="
let OR              = "or"
let AND             = "and"
let IS              = "is"
let EQ              = "="
let LTEQ            = "<="
let LT              = "<"
let GTEQ            = ">="
let GT              = ">"

// Common
let IDENTIFIER      = "[a-zA-Z_\$][a-zA-Z_\$0-9]*"
let DOT             = "."
let OPENPAREN       = "("
let CLOSEPAREN      = ")"
let OPENCURLY       = "{"
let CLOSECURLY      = "}"
let OPENSQUARE      = "["
let CLOSESQUARE     = "]"
let COLON           = ":"
let COMMA           = ","