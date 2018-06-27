module MicroCosmo.SemanticAnalyzer

open MicroCosmo

open System
open System.Collections.Generic

// Basic semantic checks
// - Create a table of all symbols (i.e. names of variables) in the program.
// - Create a table of all function calls in the program.
// - Check that functions are only defined once.
// - Check that types match (for example, check that the return statement in a function returns a type that matches the function declaration).
// - Create a table of the types of all expressions in the program.
// - Check for the existence of a main method.