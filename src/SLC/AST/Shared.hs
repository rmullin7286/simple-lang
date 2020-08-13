{-
Module      : SLC.AST.Shared
Description : Shared lexing and parsing rules for Java and SL, as well as shared types such as Identifiers
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable
-}
module SLC.AST.Shared(
    Parser,
    SpaceConsumer,
    reserved,
    Identifier(..),
    parseIdentifier,
    Name(..),
    Visibility(..),
    TypeName(..),
    Primitive(..),
    Import(..),
    Literal(..),
    parseLiteral,
    parseImport,
    unboxed,
    boxed,
    parseName,
    colon,
    lbracket,
    rbracket,
    lparen,
    rparen,
    opEquals
) where

import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T
import qualified Data.Set as S

-- |Convenience type definition for a parser rule that consumes text and returns an AST datatype
type Parser = Parsec Void T.Text

-- |A SpaceConsumer consumes space text and returns nothing. Useful for defining lexemes
type SpaceConsumer = Parsec Void T.Text ()

reservedWords :: S.Set T.Text
reservedWords = S.fromList [ "if" 
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
                      , "Int"
                      , "Double"
                      , "Float"
                      , "Bool"
                      , "Char"
                      , "Byte"
                      ]

-- |Checks if a given text value is a reserved word.
-- reserved words are shared between both Java and SL even if the word isn't
-- used in SL, just to make transpilation easier.
reserved = flip S.member reservedWords

-- |An Identifier is an unqualified name.
-- An Identifier must start with either a letter or an underscore, and may contain
-- letters, underscores and digits.
-- An identifier must not be a reserved word
newtype Identifier = Identifier T.Text

parseIdentifier :: SpaceConsumer -> Parser Identifier
parseIdentifier sc = (Lexer.lexeme sc . try) (p >>= check)
  where
    p       = T.cons <$> (letterChar <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))
    check x = if reserved x
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return $ Identifier x

-- |A name is a sequence of one or more identifiers separated by periods,
-- such as foo.bar.Baz
newtype Name = Name [Identifier]

-- |Visibility is the same in Java and SL, although SL tends to default to public visibility
data Visibility = Public | Protected | Package | Private

data TypeName = RegularName Name [TypeName]
              | PrimitiveName Primitive

data Primitive = PrimitiveInt 
               | PrimitiveBool
               | PrimitiveLong
               | PrimitiveFloat
               | PrimitiveDouble
               | PrimitiveByte
               | PrimitiveChar

data Import = Import
    { importStatic :: Bool
    , importName :: Name
    , importWildCard :: Bool
    }

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

-- all of these are stored as plain text because the translation for literals is 1 to 1
data Literal = IntLiteral T.Text
             | BoolLiteral T.Text
             | LongLiteral T.Text
             | FloatLiteral T.Text
             | DoubleLiteral T.Text
             | CharLiteral T.Text
             | StringLiteral T.Text

intLiteral :: SpaceConsumer -> Parser Literal
intLiteral sc = Lexer.lexeme sc $ IntLiteral <$> (dec <|> hex <|> bin)
  where dec = T.cons <$> (oneOf ['+','-']) <*> (T.pack <$> (some digitChar))
        hex = (<>) <$> (string "0x") <*> (T.pack <$> (some digitChar))
        bin = (<>) <$> (string "0b") <*> (T.pack <$> (some $ oneOf ['0','1']))

longLiteral :: SpaceConsumer -> Parser Literal
longLiteral sc = do
  (IntLiteral t) <- intLiteral consumeNone
  LongLiteral <$> (T.snoc t <$> char' 'l')

doubleLiteral :: SpaceConsumer -> Parser Literal
doubleLiteral sc = Lexer.lexeme sc $ do
  sign <- oneOf ['+','-']
  beforeDot <- many digitChar
  dot <- char '.'
  afterDot <- if beforeDot == "" then (some digitChar) else (many digitChar)
  return $ DoubleLiteral $ T.pack $ [sign] ++ beforeDot ++ [dot] ++ afterDot

floatLiteral :: SpaceConsumer -> Parser Literal
floatLiteral sc = Lexer.lexeme sc $ do
  (DoubleLiteral t) <- doubleLiteral consumeNone
  suffix <- char' 'f'
  return $ FloatLiteral $ T.snoc t suffix

boolLiteral :: SpaceConsumer -> Parser Literal
boolLiteral sc = Lexer.lexeme sc $ BoolLiteral <$> (string "true" <|> string "false")

-- TODO: pretty sure this is broken
charLiteral :: SpaceConsumer -> Parser Literal
charLiteral sc = Lexer.lexeme sc $ do
  char <- between (char '\'') (char '\'') Lexer.charLiteral
  return $ CharLiteral $ "'" <> (T.singleton char) <> "'"

stringLiteral :: SpaceConsumer -> Parser Literal
stringLiteral sc = Lexer.lexeme sc $ do
  str <- between (char '"') (char '"') (many Lexer.charLiteral)
  return $ StringLiteral $ T.pack str

-- TODO: support all literal formats for integers and such
parseLiteral :: SpaceConsumer -> Parser Literal
parseLiteral sc = (try $ floatLiteral sc)
                  <|> (try $ doubleLiteral sc) 
                  <|> (try $ longLiteral sc)
                  <|> (intLiteral sc)
                  <|> (boolLiteral sc)
                  <|> (charLiteral sc)
                  <|> (stringLiteral sc)

keyStatic :: SpaceConsumer -> Parser T.Text
keyStatic sc = Lexer.symbol sc "static"

keyImport :: SpaceConsumer -> Parser T.Text
keyImport sc = Lexer.symbol sc "import"

exists :: Parser T.Text -> Parser Bool
exists parser = (try parser >> return True) <|> pure False

parseName :: SpaceConsumer -> Parser Name
parseName sc = Name <$> (parseIdentifier sc) `sepBy1` char '.'

parseImport :: SpaceConsumer -> Parser Import
parseImport sc = Import <$> (keyImport sc *> exists (keyStatic sc))
                        <*> parseName sc
                        <*> exists (string ".*")

colon :: SpaceConsumer -> Parser Char
colon sc = Lexer.lexeme sc $ char ':'

lbracket :: SpaceConsumer -> Parser Char
lbracket sc = Lexer.lexeme sc $ char '{'

rbracket :: SpaceConsumer -> Parser Char
rbracket sc = Lexer.lexeme sc $ char '}'

opEquals :: SpaceConsumer -> Parser Char
opEquals sc = Lexer.lexeme sc $ char '='

lparen :: SpaceConsumer -> Parser Char
lparen sc = Lexer.lexeme sc $ char '('

rparen :: SpaceConsumer -> Parser Char
rparen sc = Lexer.lexeme sc $ char ')'