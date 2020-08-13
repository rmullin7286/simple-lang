{-
Module      : SLC.AST.Java
Description : AST definitions for Java code
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable

Since there may be collisions with AST datatypes in SL, this package should usually be imported
as qualified if it's being used alongside SL AST.
-}
module SLC.AST.Java where

import SLC.AST.Shared

data File = File
    { filePackage :: Name
    , fileClass :: Class
    }

data Class = Class
    { className :: Identifier
    , classFields :: [Field]
    , classVisibility :: Visibility
    , classConstructors :: [Constructor]
    , classMethods :: [Method]
    }

data Field = Field
    { fieldName :: Identifier
    , fieldType :: TypeName
    , fieldVisibility :: Visibility
    , fieldFinal :: Bool
    }

data Constructor = Constructor
    { constructorVisibility :: Visibility
    , constructorParams :: [Param]
    , constructorBody :: [Statement]
    }

data Method = Method
    { methodName :: Identifier
    , methodVisibility :: Visibility
    , methodParams :: [Param]
    , methodReturn :: TypeName
    , methodBody :: [Statement]
    }

data Param = Param 
    { paramName :: Identifier
    , paramType :: TypeName
    }

data Statement = Assignment Expr Expr | Return Expr

data Expr = This | FieldAccess Expr Identifier | ExprIdentifier Identifier

