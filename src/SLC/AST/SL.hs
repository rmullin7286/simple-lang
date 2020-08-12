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
    parseRecord,
    parseFile
) where

import SLC.AST.Shared
import SLC.Util
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T

-- |A Module is the top level structure of an SL program
-- Modules are either directly translated to a number of Java files or
-- to a static Java class depending on the contents
data Module = Module 
    { moduleName :: Name
    , moduleTypes :: [Record]
    }

-- |A record is a data class that translates 1 to 1 with a Java POJO
-- The declaration syntax is identical to that of F#'s records
data Record = Record
    { recordName :: Identifier
    , recordMembers :: [Field]
    }

-- |Records contain one or more fields that will be translated to fields in the final Java class
data Field = Field 
    { fieldName :: Identifier
    , fieldType :: Name
    }

sc :: SpaceConsumer
sc = Lexer.space
    (skipSome (char ' ' <|> tab))
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

allowNewline :: SpaceConsumer
allowNewline = skipMany newline

line :: Parser a -> Parser a
line parser = parser <* ((some newline >> return ()) <|> eof)

keyType :: Parser T.Text
keyType = Lexer.symbol sc "type"

separator :: Parser [Char]
separator = Lexer.lexeme sc $ some (newline <|> char ';')

parseRecord :: Parser Record
parseRecord = Record <$> (keyType *> parseIdentifier sc)
                     <*> ((opEquals sc) *> (between (lbracket sc) (rbracket sc) $ allowNewline *> field `sepEndBy` separator))
    where field = Field <$> parseIdentifier sc <* colon sc
                        <*> parseName sc

-- |Each file in a program corresponds to one Module
parseFile :: Name -> Parser Module
parseFile name = do
    types <- some $ line $ parseRecord
    return $ Module name types