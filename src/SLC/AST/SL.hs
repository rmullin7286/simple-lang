{-
Module      : SLC.AST.SL
Description : AST datatypes and parser rules for SimpleLang 
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable

Since there may be collisions with AST datatypes in Java, this package should usually be imported
as qualified if it's being used alongside Java AST.
-}
module SLC.AST.SL(
    Record(..),
    Field(..),
    Module(..),
    Function(..),
    Param(..),
    Expr(..),
    parseRecord,
    parseFile,
    parseFunction
) where

import SLC.AST.Shared
import SLC.Util
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.List

-- |A Module is the top level structure of an SL program
-- Modules are either directly translated to a number of Java files or
-- to a static Java class depending on the contents
data Module = Module 
    { moduleName :: Name
    , moduleImports :: [Import]
    , moduleTypes :: [Record]
    , moduleFunctions :: [Function]
    }

data TopLevel = TypeDecl Record | FunctionDecl Function

-- |A record is a data class that translates 1 to 1 with a Java POJO
-- The declaration syntax is identical to that of F#'s records
data Record = Record
    { recordName :: Identifier
    , recordGenerics :: [Identifier]
    , recordMembers :: [Field]
    }

-- |Records contain one or more fields that will be translated to fields in the final Java class
data Field = Field 
    { fieldName :: Identifier
    , fieldType :: TypeName
    }

data Function = Function
    { functionName :: Identifier
    , functionParams :: [Param]
    , functionReturn :: TypeName
    , functionBody :: Expr
    } deriving(Show)

data Param = Param 
    { paramName :: Identifier
    , paramType ::  TypeName
    } deriving(Show)

data Expr = LiteralExpr Literal
    deriving(Show)

parseExpr :: SpaceConsumer -> Parser Expr
parseExpr sc = LiteralExpr <$> parseLiteral sc

sc :: SpaceConsumer
sc = Lexer.space
    (skipSome (char ' ' <|> tab))
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

symbol' = Lexer.symbol sc

allowNewline :: SpaceConsumer
allowNewline = skipMany newline

line :: Parser a -> Parser a
line parser = parser <* ((some newline >> return ()) <|> eof)

keyType :: Parser T.Text
keyType = Lexer.symbol sc "type"

keyLet :: Parser T.Text
keyLet = Lexer.symbol sc "let"

separator :: Parser [Char]
separator = Lexer.lexeme sc $ some (newline <|> char ';')

parseFunction = Function <$> (keyLet *> parseIdentifier sc)
                         <*> some parseParam
                         <*> (colon sc *> parseTypeName)
                         <*> (opEquals sc *> parseExpr sc)

parseParam :: Parser Param
parseParam =  between (lparen sc) (rparen sc) $ Param <$> parseIdentifier sc 
                                                      <*> (colon sc *> parseTypeName)
                         

parseRecord :: Parser Record
parseRecord = Record <$> (keyType *> parseIdentifier sc)
                     <*> (many $ parseIdentifier sc)
                     <*> ((opEquals sc) *> (between (lbracket sc) (rbracket sc) $ allowNewline *> field `sepEndBy` separator))
    where field = Field <$> parseIdentifier sc <* colon sc
                        <*> parseTypeName

parseTypeName :: Parser TypeName
parseTypeName = try parsePrimitiveName <|> parseRegularName

parsePrimitiveName :: Parser TypeName
parsePrimitiveName = PrimitiveName <$> (primInt <|> primLong <|> primDouble <|> primFloat <|> primChar <|> primByte)
    where primInt = symbol' "Int" >> return PrimitiveInt
          primLong = symbol' "Long" >> return PrimitiveLong
          primDouble = symbol' "Double" >> return PrimitiveDouble
          primFloat = symbol' "Float" >> return PrimitiveFloat
          primChar = symbol' "Char" >> return PrimitiveChar
          primByte = symbol' "Byte" >> return PrimitiveByte

parseRegularName :: Parser TypeName
parseRegularName = RegularName <$> parseName sc <*> many param 
    where param = parentheses <|> plainName
          plainName = (flip RegularName []) <$> parseName sc
          parentheses = between (lparen sc) (rparen sc) parseTypeName

parseTopLevel :: Parser TopLevel
parseTopLevel = (TypeDecl <$> parseRecord) <|> (FunctionDecl <$> parseFunction)

partitionTopLevels :: [TopLevel] -> ([Record], [Function])
partitionTopLevels = partitionMap $ \case (TypeDecl t) -> Left t
                                          (FunctionDecl f) -> Right f

-- |Each file in a program corresponds to one Module
parseFile :: Name -> Parser Module
parseFile name = do
    imports <- many $ line $ parseImport sc
    topLevels <- some $ line $ parseTopLevel
    let (types, functions) = partitionTopLevels topLevels
    return $ Module name imports types functions