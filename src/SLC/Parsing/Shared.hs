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

consumeNone :: SpaceConsumer
consumeNone = return ()

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

-- TODO: this doesn't support all possible forms of int literals
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