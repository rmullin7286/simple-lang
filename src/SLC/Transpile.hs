{-
Module      : SLC.Transpile
Description : This module contains various routines for the transpilation step of the compiler
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable

The transpilation steps for SL -> Java are as follows:
    1. Convert each type in the Module into a java file containing a class corresponding to that type
    2. Each member of an SL record will be converted to a field of the same name and a getter method
    3. Each free function in an SL unit will be associated with one of the java classes and added to it. Free functions
       are associated by searching the parameters in the signature for the first one that has a type contained in the module.
    4. If no matching type is found for any functions the compiler will fail. Later in development handling of separate and static methods will be done.
-}
module SLC.Transpile where

import SLC.AST.Shared
import SLC.Util
import qualified SLC.AST.SL as SL
import qualified SLC.AST.Java as J
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List

transpileModule :: SL.Module -> [J.File]
transpileModule (SL.Module name imports types functions) =
    let basicClasses = createBasicClasses types
        classMap = M.fromList $ map (\c@(J.Class n _ _ _ _ _) -> (n, c)) $ basicClasses
        (leftoverFunctions, classesWithMethods) = attachFunctions classMap functions
    in if empty leftoverFunctions
       then map (J.File name imports) classesWithMethods
       else error $ "Could not associate some functions"

createBasicClasses :: [SL.Record] -> [J.Class]
createBasicClasses = map (\(SL.Record name generics fields) -> J.Class
    name
    generics
    (map transpileField fields)
    Public
    [createBasicConstructor fields]
    (map getter fields))

transpileField :: SL.Field -> J.Field
transpileField (SL.Field name typen) = J.Field name typen Private True

createBasicConstructor :: [SL.Field] -> J.Constructor
createBasicConstructor fields = J.Constructor Public params body
    where params = map (\(SL.Field name typen) -> J.Param name typen True) fields
          body = map (\(SL.Field name _) -> J.Assignment (J.FieldAccess J.This name) (J.ExprIdentifier name)) fields

getter :: SL.Field -> J.Method
getter (SL.Field iname@(Identifier name) typen) = J.Method 
    { methodName = Identifier $ "get" <> T.toTitle name
    , methodVisibility = Public
    , methodParams = []
    , methodReturn = typen
    , methodBody = [J.Return $ J.FieldAccess J.This iname]
    }

attachFunctions :: M.Map Identifier J.Class -> [SL.Function] -> ([SL.Function], [J.Class])
attachFunctions classMap fs = attachFunctions' fs classMap []
    where attachFunctions' :: [SL.Function] -> M.Map Identifier J.Class -> [SL.Function] -> ([SL.Function], [J.Class])
          attachFunctions' [] newMap orphans = (orphans, M.elems newMap)
          attachFunctions' ((f@SL.Function{..}):fs) newMap orphans = 
            let paramIndex = findIndex (\(SL.Param n typen) -> case typen of
                                                                  RegularName name args -> (unqualified name) `M.member` newMap
                                                                  _ -> False) functionParams
            in case paramIndex of 
                Just idx -> 
                    let className = (unqualifiedTypename . SL.paramType . (functionParams !!)) idx
                        newArgs = removeAt idx functionParams
                        newMap' = M.adjust (\theClass -> J.addMethod theClass (createMethod functionName newArgs functionReturn functionBody)) className newMap
                    in attachFunctions' fs newMap' orphans
                Nothing -> attachFunctions' fs newMap (f : orphans)
        

createMethod :: Identifier -> [SL.Param] -> TypeName -> SL.Expr -> J.Method
createMethod name params return body = J.Method
    name
    Public
    (map (\(SL.Param name typen) -> J.Param name typen True) params)
    return 
    (transpileBody body)

transpileBody :: SL.Expr -> [J.Statement]
transpileBody (SL.LiteralExpr l) = [J.Return $ J.LiteralExpr l]
                

-- transpileToFile :: Name -> [Import] -> SL.Record -> J.File
-- transpileToFile package imports typeDecl = J.File package imports (transpileToClass typeDecl)

-- transpileToClass :: SL.Record -> J.Class
-- transpileToClass (SL.Record ident generics members) = J.Class
--     { className = ident
--     , classFields = recordFieldsToJavaFields members
--     , classVisibility = Public
--     , classConstructors = recordFieldsToConstructors members
--     , classMethods = recordFieldsToMethods members
--     , classGenerics = generics
--     }


-- recordFieldsToJavaFields :: [SL.Field] -> [J.Field]
-- recordFieldsToJavaFields = map $ \(SL.Field name typen) -> J.Field 
--     { fieldName = name
--     , fieldType = typen
--     , fieldVisibility = Private
--     , fieldFinal = True
--     }

-- recordFieldsToConstructors :: [SL.Field] -> [J.Constructor]
-- recordFieldsToConstructors fields = [J.Constructor Public params statements]
--     where params = map (\(SL.Field name typen) -> J.Param name typen) fields
--           statements = map (\(SL.Field name _) -> J.Assignment (J.FieldAccess J.This name) (J.ExprIdentifier name)) fields

-- recordFieldsToMethods :: [SL.Field] -> [J.Method]
-- recordFieldsToMethods = map getter

