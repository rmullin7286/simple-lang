module SLC.AST.Java where

import SLC.AST.Shared
import qualified Data.Text as T

data CompilationUnit = CompilationUnit Name [Import] TypeDecl

data TypeDecl = ClassTypeDecl Class

data Class = Class
    { classFinal :: Bool
    , classAccess :: Access
    , className :: Identifier
    , classFields :: [Field]
    , classConstructors :: [Constructor]
    , classMethods :: [Method]
    }

data Field = Field
    { fieldAccess :: Access
    , fieldStatic :: Bool
    , fieldFinal :: Bool
    , fieldName :: Identifier
    , fieldType :: TypeName
    }

data Constructor = Constructor
    { constructorAccess :: Access
    , constructorParams :: [Param]
    , constructorBody :: [Statement]
    }

data Method = Method
    { methodAccess :: Access
    , methodStatic :: Bool
    , methodFinal :: Bool
    , methodReturnType :: TypeName
    , methodName :: Identifier
    , methodParams :: [Param]
    , methodBody :: [Statement]
    }

newtype Statement = Statement T.Text

data Param = Param
    { paramFinal :: Bool
    , paramType :: TypeName
    , paramName :: Identifier
    }
