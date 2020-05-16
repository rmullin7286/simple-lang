module SLC.Parsing.Shared where

import SLC.AST.Shared

import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Char hiding()
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T
import qualified Data.Set as S

type Parser = Parsec Void T.Text

type SpaceConsumer = Parsec Void T.Text ()

reserved :: S.Set T.Text
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
                      , "Int"
                      , "Double"
                      , "Float"
                      , "Bool"
                      , "Char"
                      , "Byte"
                      ]

keyStatic :: SpaceConsumer -> Parser T.Text
keyStatic sc = Lexer.symbol sc "static"

keyImport :: SpaceConsumer -> Parser T.Text
keyImport sc = Lexer.symbol sc "import"

exists :: Parser T.Text -> Parser Bool
exists parser = (try parser >> return True) <|> pure False

parseIdentifier :: SpaceConsumer -> Parser Identifier
parseIdentifier sc = (Lexer.lexeme sc . try) (p >>= check)
  where
    p       = T.cons <$> (letterChar <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))
    check x = if x `S.member` reserved
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return $ Identifier x

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