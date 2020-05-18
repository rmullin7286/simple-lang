module SLC.AST.Shared where

import qualified Data.Text as T
import Data.Word

newtype Identifier = Identifier {unIdentifier :: T.Text}

newtype Name = Name [Identifier]

(<.>) :: Name -> Identifier -> Name
(Name idents) <.> ident = Name $ idents ++ [ident]

data TypeName = RegularName Name [TypeName]
              | PrimitiveName Primitive

data Primitive = PrimitiveInt 
               | PrimitiveBool
               | PrimitiveLong
               | PrimitiveFloat
               | PrimitiveDouble
               | PrimitiveByte
               | PrimitiveChar

simpleRegName t = RegularName (Name [Identifier t]) []

unboxed :: Primitive -> TypeName
unboxed PrimitiveInt = simpleRegName "int"
unboxed PrimitiveBool = simpleRegName "bool"
unboxed PrimitiveLong = simpleRegName "long"
unboxed PrimitiveFloat = simpleRegName "float"
unboxed PrimitiveDouble = simpleRegName "double"
unboxed PrimitiveByte = simpleRegName "byte"
unboxed PrimitiveChar = simpleRegName "char"

boxed :: Primitive -> TypeName
boxed PrimitiveInt = simpleRegName "Int"
boxed PrimitiveBool = simpleRegName "Bool"
boxed PrimitiveLong = simpleRegName "Long"
boxed PrimitiveFloat = simpleRegName "Float"
boxed PrimitiveDouble = simpleRegName "Double"
boxed PrimitiveByte = simpleRegName "Byte"
boxed PrimitiveChar = simpleRegName "Char"

data Import = Import
    { importStatic :: Bool
    , importName :: Name
    , importWildCard :: Bool
    }

data Access = Public | Private | Package | Protected

-- all of these are stored as plain text because the translation for literals is 1 to 1
data Literal = IntLiteral T.Text
             | BoolLiteral T.Text
             | LongLiteral T.Text
             | FloatLiteral T.Text
             | DoubleLiteral T.Text
             | CharLiteral T.Text
             | StringLiteral T.Text