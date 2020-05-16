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

allowNewline :: SpaceConsumer
allowNewline = skipMany newline

separator :: Parser [Char]
separator = Lexer.lexeme sc $ some (newline <|> char ';')

line :: Parser a -> Parser a
line parser = parser <* ((some newline >> return ()) <|> eof)

parseSLTypeName :: Parser TypeName
parseSLTypeName = TypeName <$> parseName sc <*> many param 
    where param = parentheses <|> plainName
          plainName = (flip TypeName []) <$> parseName sc
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