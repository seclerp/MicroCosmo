module MicroCosmo.SymbolScopeStack

open MicroCosmo
open MicroCosmo.SymbolScope
open MicroCosmo.CompilerErrors

open System
open System.Collections.Generic

type SymbolScopeStack() =
    let stack = new Stack<SymbolScope>()
    do stack.Push(new SymbolScope(None))
    
    member x.CurrentScope = stack.Peek()
    
    member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
    member x.Pop() = stack.Pop() |> ignore
    member x.AddDeclaration declaration = stack.Peek().AddDeclaration declaration