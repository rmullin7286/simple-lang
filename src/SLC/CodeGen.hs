module SLC.CodeGen where

import Control.Monad.Writer
import Control.Monad
import SLC.AST.Java
import SLC.AST.Shared
import qualified Data.Text as T

type JavaGen = Writer T.Text ()

runJavaGen :: JavaGen -> T.Text
runJavaGen generator = execWriter $ (writer ((), "")) >> generator

raw :: T.Text -> JavaGen
raw = tell

postProcess :: (T.Text -> T.Text) -> JavaGen -> JavaGen
postProcess = censor

line :: JavaGen -> JavaGen
line gen = gen >> raw ";\n"

emptyLines :: Int -> JavaGen
emptyLines n = replicateM_ n $ raw "\n"

intercalateGen :: T.Text -> (a -> JavaGen) -> [a] -> JavaGen
intercalateGen _ _ [] = raw ""
intercalateGen _ f (x:[]) = f x
intercalateGen sep f (x:xs) = f x >> raw sep >> intercalateGen sep f xs

statement :: Statement -> JavaGen
statement (Statement t) = raw t >> raw ";\n"

withBody :: JavaGen -> JavaGen -> JavaGen 
withBody expr body = do
    expr >> raw " {\n"
    indent body
    raw "}\n"

indent :: JavaGen -> JavaGen
indent = postProcess (\text -> T.dropEnd 2 ("  " <> T.replace "\n" "\n  " text)) -- there's probably a better way to do this

genPackageDecl :: Name -> JavaGen
genPackageDecl name = line $ raw "package " >> genName name

genName :: Name -> JavaGen
genName (Name idents) = intercalateGen "." (\(Identifier i) -> raw i) idents

genIdentifier :: Identifier -> JavaGen
genIdentifier (Identifier t) = raw t

genImport :: Import -> JavaGen
genImport (Import static name wild) = line $ do
    raw "import "
    when static $ raw "static "
    genName name
    when wild $ raw ".*"

genTypeDecl :: TypeDecl -> JavaGen
genTypeDecl (ClassTypeDecl c) = genClass c

genClass :: Class -> JavaGen
genClass clazz@Class{..} = do
    let header = do
            genAccess classAccess
            raw " "
            when classFinal $ raw "final "
            raw "class "
            genIdentifier className
    withBody header $ do
        forM_ classFields genField
        emptyLines 1
        forM_ classConstructors $ \c -> genConstructor className c >> emptyLines 1
        forM_ classMethods $ \m -> genMethod m >> emptyLines 1

genField :: Field -> JavaGen
genField Field{..} = line $ do
    genAccess fieldAccess
    raw " "
    when fieldFinal $ raw "final "
    when fieldStatic $ raw "static "
    genTypeName fieldType
    raw " "
    genIdentifier fieldName


genAccess :: Access -> JavaGen
genAccess Public = raw "public"
genAccess Private = raw "private"
genAccess Package = raw ""
genAcess Protected = raw "protected"

genTypeName :: TypeName -> JavaGen
genTypeName (TypeName name []) = genName name
genTypeName (TypeName name args) = do
    genName name
    raw "<"
    intercalateGen "," genTypeName args

genMethod :: Method -> JavaGen
genMethod Method{..} = do
    let header = do
            genAccess methodAccess
            raw " "
            when methodFinal $ raw "final "
            when methodStatic $ raw "static "
            genTypeName methodReturnType
            raw " "
            genIdentifier methodName
            genParameters methodParams
    withBody header $ forM_ methodBody statement

genConstructor :: Identifier -> Constructor -> JavaGen
genConstructor name Constructor{..} = do
    let header = do
            genAccess constructorAccess
            genIdentifier name
            genParameters constructorParams
    withBody header $ forM_ constructorBody statement

genParameters :: [Param] -> JavaGen
genParameters params = do
    raw "( "
    let genParam (Param f t n) = do
                when f $ raw "final "
                genTypeName t
                raw " "
                genIdentifier n
    intercalateGen ", " genParam params
    raw " )"

genCompilationUnit :: CompilationUnit -> JavaGen
genCompilationUnit (CompilationUnit name imports typeDecl) = do
    genPackageDecl name
    emptyLines 1
    forM_ imports genImport
    emptyLines
    genTypeDecl typeDecl