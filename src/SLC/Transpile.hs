{-
Module      : SLC.Transpile
Description : This module contains various routines for the transpilation step of the compiler
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable
-}
module SLC.Transpile where

import SLC.AST.Shared
import qualified SLC.AST.SL as SL
import qualified SLC.AST.Java as J
import qualified Data.Text as T

transpileModule :: SL.Module -> [J.File]
transpileModule (SL.Module name imports types) = map (transpileToFile name imports) types

transpileToFile :: Name -> [Import] -> SL.Record -> J.File
transpileToFile package imports typeDecl = J.File package imports (transpileToClass typeDecl)

transpileToClass :: SL.Record -> J.Class
transpileToClass (SL.Record ident generics members) = J.Class
    { className = ident
    , classFields = recordFieldsToJavaFields members
    , classVisibility = Public
    , classConstructors = recordFieldsToConstructors members
    , classMethods = recordFieldsToMethods members
    , classGenerics = generics
    }


recordFieldsToJavaFields :: [SL.Field] -> [J.Field]
recordFieldsToJavaFields = map $ \(SL.Field name typen) -> J.Field 
    { fieldName = name
    , fieldType = typen
    , fieldVisibility = Private
    , fieldFinal = True
    }

recordFieldsToConstructors :: [SL.Field] -> [J.Constructor]
recordFieldsToConstructors fields = [J.Constructor Public params statements]
    where params = map (\(SL.Field name typen) -> J.Param name typen) fields
          statements = map (\(SL.Field name _) -> J.Assignment (J.FieldAccess J.This name) (J.ExprIdentifier name)) fields

recordFieldsToMethods :: [SL.Field] -> [J.Method]
recordFieldsToMethods = map getter

getter :: SL.Field -> J.Method
getter (SL.Field iname@(Identifier name) typen) = J.Method 
    { methodName = Identifier $ "get" <> T.toTitle name
    , methodVisibility = Public
    , methodParams = []
    , methodReturn = typen
    , methodBody = [J.Return $ J.FieldAccess J.This iname]
    }