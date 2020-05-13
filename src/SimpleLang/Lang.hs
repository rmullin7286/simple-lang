module SimpleLang.Lang
       ( Identifier(..)
       , Name(..)
       , Access(..)
       , Import(..)
       , Module(..)
       , TypeDecl(..)
       , TypeName(..)
       , RecordMember(..)
       , TopLevelDecl(..)
       , identifier
       , name
       , importStmt
       , typeName
       , simpleParse
       , parseModule
       , typeDecl
       , topLevel
       , line
       ) where

import Data.Text(Text)
import qualified Data.Text as T
import TextShow
import TextShow.Generic
import Data.Set(Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import GHC.Generics
import SimpleLang.Parser


newtype Identifier = Identifier Text
    deriving Show via (FromTextShow Identifier)
    deriving(Eq)

instance TextShow Identifier where
    showb (Identifier t) = fromText t

newtype Name = Name [Identifier]
    deriving Show via (FromTextShow Name)

instance Semigroup Name where
    (Name xs) <> (Name ys) = Name $ xs <> ys

data TypeName = TypeName Name [TypeName]
    deriving(Show)

instance TextShow Name where
    showb (Name idents) = fromText $ T.intercalate "." $ map showt idents


data Access = Public | Private | Package | Protected
    deriving(Generic)
    deriving TextShow via (FromGeneric Access)

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

data TypeDecl = RecordDecl Identifier [RecordMember]
    deriving(Show)

data FunDecl = FunDecl
    deriving(Show)

data RecordMember = RecordMember Identifier TypeName
    deriving(Show)

data Module = Module Name [Import] [TypeDecl] [FunDecl]
    deriving(Show)

data TopLevelDecl = TopLevelType TypeDecl


reserved :: Set Text
reserved = S.fromList [ "if" 
                      , "then"
                      , "else"
                      , "static"
                      , "import"
                      , "public"
                      , "private"
                      , "package"
                      , "protected"
                      , "type"
                      , "module"
                      ]

throwaway parser = parser >> return ()

-- parser definitions
ignorableSpaces :: Parser ()
ignorableSpaces = skipSome (char ' ' <|> tab)

line :: Parser a -> Parser a
line parser = parser <* ((some newline >> return ()) <|> eof)

ignore :: Parser ()
ignore = L.space
    ignorableSpaces
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

exists :: Parser Text -> Parser Bool
exists parser = (try parser >> return True) <|> pure False

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ignore

symbol :: Text -> Parser Text
symbol = L.symbol ignore

keyImport :: Parser Text
keyImport = symbol "import"

keyStatic :: Parser Text
keyStatic = symbol "static"

keyType :: Parser Text
keyType = symbol "type"

allowNewline :: Parser ()
allowNewline = skipMany newline

lbracket :: Parser Char
lbracket = lexeme $ char '{'

rbracket :: Parser Char
rbracket = lexeme $ char '}'

lsquareBracket :: Parser Char
lsquareBracket = lexeme $ char '['

rsquareBracket :: Parser Char
rsquareBracket = lexeme $ char ']'

lparen :: Parser Char
lparen = lexeme $ char '('

rparen :: Parser Char
rparen = lexeme $ char ')'

opEquals :: Parser Char
opEquals = lexeme $ char '='

colon :: Parser Char
colon = lexeme $ char ':'

separator :: Parser [Char]
separator = lexeme $ some (newline <|> char ';')

identifier :: Parser Identifier
identifier = (lexeme . try) (p >>= check)
  where
    p       = T.cons <$> (letterChar <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))
    check x = if x `S.member` reserved
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return $ Identifier x

name :: Parser Name
name = Name <$> (identifier) `sepBy1` char '.'

importStmt :: Parser Import
importStmt = Import <$> (keyImport *> exists keyStatic)
                           <*> name 
                           <*> exists (string ".*")

typeName :: Parser TypeName
typeName = TypeName <$> name <*> many param 
    where param = parentheses <|> plainName
          plainName = (flip TypeName []) <$> name
          parentheses = between lparen rparen typeName

typeDecl :: Parser TypeDecl
typeDecl = recordDecl

recordDecl :: Parser TypeDecl
recordDecl = RecordDecl <$> (keyType *> identifier)
                        <*> (opEquals *> (between lbracket rbracket $ allowNewline *> recordMember `sepEndBy` separator))
    where recordMember = RecordMember <$> identifier <* colon 
                                      <*> typeName

topLevel :: Parser TopLevelDecl
topLevel = TopLevelType <$> typeDecl

parseModule :: Name  -> Parser Module
parseModule name = do
    let isTypeDecl tl = case tl of
                            TopLevelType _ -> True
                            _ -> False
    let getTypes = map (\(TopLevelType t) -> t) . filter isTypeDecl

    imports <- many $ line importStmt
    topLevel <- (some $ line topLevel)
    return $ Module name imports (getTypes topLevel) []


simpleParse parser = parse parser ""