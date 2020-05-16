module SLC.Transpile where

import SLC.AST.SimpleLang
import SLC.AST.Shared
import SLC.AST.Java
import SLC.Util

transpileModule :: Module -> [CompilationUnit]
transpileModule (Module name imports types) = map (\t -> CompilationUnit name imports $ transpileType t) types

transpileType (RecordDecl name members) = ClassTypeDecl $ Class
    True
    Public
    name
    (recordMemberFields members)
    (recordMemberConstructors members)
    (recordMemberMethods members)

recordMemberConstructors members = [Constructor Public params statements]
    where params = map (\(RecordMember i n) -> Param True n i) members
          names = map (\(RecordMember i _) -> i) members
          statements = map (\(Identifier name) -> Statement $ "this." <> name <> " = " <> name) names

recordMemberFields = map $ \(RecordMember ident typen) -> Field Private False True ident typen

recordMemberMethods = map $ \(RecordMember name@(Identifier ident) typen) -> Method 
    Public 
    False 
    True 
    typen 
    (getterName name)
    []
    [Statement $ "return this." <> ident]

getterName (Identifier t) = Identifier $ "get" <> capitalizeFirst t
