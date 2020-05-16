module SLC.AST.Shared where

import qualified Data.Text as T

newtype Identifier = Identifier {unIdentifier :: T.Text}

newtype Name = Name [Identifier]

(<.>) :: Name -> Identifier -> Name
(Name idents) <.> ident = Name $ idents ++ [ident]

data TypeName = TypeName Name [TypeName]

data Import = Import
    { importStatic :: Bool
    , importName :: Name
    , importWildCard :: Bool
    }

data Access = Public | Private | Package | Protected