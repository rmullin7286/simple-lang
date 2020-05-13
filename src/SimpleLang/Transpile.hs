module SimpleLang.Transpile(transpile) where

import qualified SimpleLang.Lang as SL
import qualified SimpleLang.Java as J
import SimpleLang.Util
import TextShow

transpileIdentifier :: SL.Identifier -> J.Identifier
transpileIdentifier (SL.Identifier t) = J.Identifier t

transpileName :: SL.Name -> J.Name
transpileName (SL.Name idents) = J.Name (map transpileIdentifier idents)

transpileTypeName :: SL.TypeName -> J.TypeName
transpileTypeName (SL.TypeName n args) = J.TypeName (transpileName n) (map transpileTypeName args)

transpileImport :: SL.Import -> J.Import
transpileImport (SL.Import s n w) = J.Import s (transpileName n) w

transpile :: SL.Module -> [J.CompilationUnit]
transpile (SL.Module name imports [typeDecl] _) = [J.CompilationUnit (transpileName name) (map transpileImport imports) (transpileType typeDecl)]
transpile (SL.Module name imports typeDecls _) = map go typeDecls
    where go typeDecl = J.CompilationUnit (transpileName name) (map transpileImport imports) (transpileType typeDecl)

transpileType :: SL.TypeDecl -> J.TypeDecl
transpileType (SL.RecordDecl name members) = J.ClassTypeDecl $ J.Class
    True
    J.Public
    (transpileIdentifier name)
    (recordMemberFields members)
    (recordMemberConstructors members)
    (recordMemberMethods members)

recordMemberFields :: [SL.RecordMember] -> [J.Field]
recordMemberFields = 
    map $ \(SL.RecordMember ident typen) -> J.Field J.Private False True (transpileIdentifier ident) (transpileTypeName typen)

recordMemberConstructors :: [SL.RecordMember] -> [J.Constructor]
recordMemberConstructors members = [J.Constructor J.Public params statements]
    where params = map (\(SL.RecordMember i n) -> J.Param True (transpileTypeName n) (transpileIdentifier i)) members
          names = map (\(SL.RecordMember i _) -> i) members
          statements = map (\(SL.Identifier name) -> J.Statement $ "this." <> name <> " = " <> name) names
    
recordMemberMethods :: [SL.RecordMember] -> [J.Method]
recordMemberMethods =
    map $ \mem@(SL.RecordMember ident typen) -> J.Method J.Public False True (transpileTypeName typen) (recordMemberGetterName mem) [] [J.Statement $ "return this." <> showt ident]

-- TODO: handle bools
recordMemberGetterName :: SL.RecordMember -> J.Identifier
recordMemberGetterName (SL.RecordMember (SL.Identifier i) _) = J.Identifier $ "get" <> (capitalizeFirst i)