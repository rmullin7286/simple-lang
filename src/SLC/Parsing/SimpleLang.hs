module SLC.Parsing.SimpleLang where

import SLC.Parsing.Shared
import SLC.AST.SimpleLang
import SLC.AST.Shared
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec
import Text.Megaparsec.Char

sc :: SpaceConsumer
sc = Lexer.space
    (skipSome (char ' ' <|> tab))
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

keyType = Lexer.symbol sc "type"

symbol' = Lexer.symbol sc

allowNewline :: SpaceConsumer
allowNewline = skipMany newline

separator :: Parser [Char]
separator = Lexer.lexeme sc $ some (newline <|> char ';')

line :: Parser a -> Parser a
line parser = parser <* ((some newline >> return ()) <|> eof)

parseSLTypeName :: Parser TypeName
parseSLTypeName = try parseSLPrimitiveName <|> parseSLRegularName

parseSLPrimitiveName :: Parser TypeName
parseSLPrimitiveName = PrimitiveName <$> (primInt <|> primLong <|> primDouble <|> primFloat <|> primChar <|> primByte)
    where primInt = symbol' "Int" >> return PrimitiveInt
          primLong = symbol' "Long" >> return PrimitiveLong
          primDouble = symbol' "Double" >> return PrimitiveDouble
          primFloat = symbol' "Float" >> return PrimitiveFloat
          primChar = symbol' "Char" >> return PrimitiveChar
          primByte = symbol' "Byte" >> return PrimitiveByte

parseSLRegularName :: Parser TypeName
parseSLRegularName = RegularName <$> parseName sc <*> many param 
    where param = parentheses <|> plainName
          plainName = (flip RegularName []) <$> parseName sc
          parentheses = between (lparen sc) (rparen sc) parseSLTypeName

parseTypeDecl :: Parser TypeDecl
parseTypeDecl = parseRecordDecl

parseRecordDecl :: Parser TypeDecl
parseRecordDecl = RecordDecl <$> (keyType *> parseIdentifier sc)
                        <*> ((opEquals sc) *> (between (lbracket sc) (rbracket sc) $ allowNewline *> recordMember `sepEndBy` separator))
    where recordMember = RecordMember <$> parseIdentifier sc <* colon sc
                                      <*> parseSLTypeName

parseTopLevelDecl :: Parser TopLevelDecl
parseTopLevelDecl = TopLevelType <$> parseTypeDecl

parseModule :: Name  -> Parser Module
parseModule name = do
    let isTypeDecl tl = case tl of
                            TopLevelType _ -> True
                            _ -> False
    let getTypes = map (\(TopLevelType t) -> t) . filter isTypeDecl

    imports <- many $ line (parseImport sc)
    topLevel <- (some $ line parseTopLevelDecl)
    return $ Module name imports (getTypes topLevel)