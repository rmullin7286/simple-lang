{-
Module      : SLC.CodeGen
Description : This module has useful monadic constructs for generating Java code from AST
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable
-}
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

indent :: JavaGen -> JavaGen
indent = postProcess (\text -> T.dropEnd 2 ("  " <> T.replace "\n" "\n  " text)) -- there's probably a better way to do this

withBody :: JavaGen -> JavaGen -> JavaGen 
withBody expr body = do
    expr >> raw " {\n"
    indent body
    raw "}\n"

genPackageDecl :: Name -> JavaGen
genPackageDecl name = line $ raw "package " >> genName name

genVisibility :: Visibility -> JavaGen
genVisibility Public = raw "public"
genVisibility Private = raw "private"
genVisibility Package = raw ""
genAcess Protected = raw "protected"

genIdentifier :: Identifier -> JavaGen
genIdentifier (Identifier t) = raw t

genName :: Name -> JavaGen
genName (Name idents) = intercalateGen "." (\(Identifier i) -> raw i) idents

genImport :: Import -> JavaGen
genImport (Import static name wildcard) = line $ do
    raw "import "
    when static $ raw "static "
    genName name
    when wildcard $ raw ".*"

genTypeName :: TypeName -> JavaGen
genTypeName (PrimitiveName primitive) = genName name
    where (RegularName name _) = unboxed primitive
genTypeName (RegularName name []) = genName name
genTypeName (RegularName name args) = do
    genName name
    raw "<"
    intercalateGen "," genTypeName args

genField :: Field -> JavaGen
genField Field{..} = line $ do
    genVisibility fieldVisibility
    raw " "
    when fieldFinal $ raw "final "
    genTypeName fieldType
    raw " "
    genIdentifier fieldName

genParameters :: [Param] -> JavaGen
genParameters [] = raw "()"
genParameters params = do
    raw "( "
    let genParam (Param name typen) = do
                genTypeName typen
                raw " "
                genIdentifier name
    intercalateGen ", " genParam params
    raw " )"

genConstructor :: Identifier -> Constructor -> JavaGen
genConstructor name Constructor{..} = do
    let header = do
            genVisibility constructorVisibility
            raw " "
            genIdentifier name
            genParameters constructorParams
    withBody header $ mapM_ genStatement constructorBody

genMethod :: Method -> JavaGen
genMethod Method{..} = do
    let header = do
            genVisibility methodVisibility
            raw " "
            genTypeName methodReturn
            raw " "
            genIdentifier methodName
            genParameters methodParams
    withBody header $ mapM_ genStatement methodBody

genStatement :: Statement -> JavaGen
genStatement (Assignment lhs rhs) = do
    genExpr lhs
    raw " = "
    genExpr rhs
    raw ";\n"
genStatement (Return expr) = do
    raw "return "
    genExpr expr
    raw ";\n"

genExpr :: Expr -> JavaGen
genExpr This = raw "this"
genExpr (FieldAccess lhs ident) = genExpr lhs >> raw "." >> genIdentifier ident
genExpr (ExprIdentifier ident) = genIdentifier ident

genClass :: Class -> JavaGen
genClass Class{..} = do
    let header = do
            genVisibility classVisibility
            raw " class "
            genIdentifier className
    withBody header $ do
        forM_ classFields genField
        emptyLines 1
        forM_ classConstructors $ \c -> genConstructor className c >> emptyLines 1
        forM_ classMethods $ \m -> genMethod m >> emptyLines 1


genFile :: File -> JavaGen
genFile File{..} = do
    genPackageDecl filePackage
    emptyLines 1
    mapM_ genImport fileImports
    emptyLines 1
    genClass fileClass
    