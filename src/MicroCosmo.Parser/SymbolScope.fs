module MicroCosmo.SymbolScope

open MicroCosmo
open MicroCosmo.CompilerErrors

open System
open System.Collections.Generic

type SymbolScope(parent : SymbolScope option) =
    let mutable list = List.empty<Ast.VariableDeclarationStatement>
    
    let identifierFromDeclaration =
        function
        | (i, _, _, _) -> i
    
    let declaresIdentifier (identifierRef : Ast.IdentifierRef) declaration =
        (identifierFromDeclaration declaration) = identifierRef.Identifier
    
    member x.AddDeclaration declaration =
        let ifd = identifierFromDeclaration
        if List.exists (fun x -> ifd x = ifd declaration) list then
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