module SimpleLang.Java
       ( CompilationUnit(..)
       , Import(..)
       , Identifier(..)
       , Name(..)
       , TypeDecl(..)
       , Class(..)
       , Field(..)
       , Method(..)
       , Constructor(..)
       , Param(..)
       , Access(..)
       , TypeName(..)
       , Statement(..)
       ) where

import Data.Text(Text)
import qualified Data.Text as T
import TextShow
import GHC.Generics hiding(Constructor)
import TextShow.Generic

newtype Identifier = Identifier Text
    deriving Show via (FromTextShow Identifier)

instance TextShow Identifier where
    showb (Identifier t) = fromText t

newtype Name = Name [Identifier]
    deriving Show via (FromTextShow Name)

instance TextShow Name where
    showb (Name idents) = fromText $ T.intercalate "." $ map showt idents

data TypeName = TypeName Name [TypeName]

instance TextShow TypeName where
    showb (TypeName n []) = showb n
    showb (TypeName n args) = showb n <> "<" <> (fromText $ T.intercalate "," $ map showt args) <> ">"

data CompilationUnit = CompilationUnit Name [Import] TypeDecl

data TypeDecl = ClassTypeDecl Class

data Access = Public | Private | Package | Protected

data Class = Class
    { classIsFinal :: Bool
    , classAccess :: Access
    , className :: Identifier
    , classFields :: [Field]
    , classConstructors :: [Constructor]
    , classMethods :: [Method]
    }

data Field = Field
    { fAccess :: Access
    , fIsStatic :: Bool
    , fIsFinal :: Bool
    , fName :: Identifier
    , fType :: TypeName
    }

data Constructor = Constructor
    { cAccess :: Access
    , cParams :: [Param]
    , cBody :: [Statement]
    }

data Method = Method
    { mAccess :: Access
    , mIsStatic :: Bool
    , mIsFinal :: Bool
    , mReturnType :: TypeName
    , mName :: Identifier
    , mParams :: [Param]
    , mBody :: [Statement]
    }

newtype Statement = Statement Text

data Param = Param
    { pIsFinal :: Bool
    , pType :: TypeName
    , pName :: Identifier
    }

data Import = Import 
    { iStatic :: Bool
    , iName :: Name
    , iWildCard :: Bool
    } deriving Show via (FromTextShow Import)

instance TextShow Import where
    showb (Import s n w) = "import "
                           <> (if s then "static " else "") 
                           <> showb n
                           <> (if w then ".*" else "")