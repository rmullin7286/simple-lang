module SimpleLang.CodeGen where

import Control.Monad.Writer
import Control.Monad
import SimpleLang.Java
import Data.Text(Text)
import qualified Data.Text as T
import SimpleLang.Util
import TextShow

type JavaGen = Writer Text ()

runJavaGen :: JavaGen -> Text
runJavaGen generator = execWriter $ (writer ((), "")) >> generator

line :: Text -> JavaGen
line contents = tell (contents <> "\n")

emptyLines :: Int -> JavaGen
emptyLines n = replicateM_ n $ tell "\n"

statement :: Text -> JavaGen
statement contents = tell (contents <> ";\n")

withBody :: JavaGen -> JavaGen -> JavaGen 
withBody expr body = do
    censor (\text -> text <> " {\n") expr
    indent body
    line "}"

raw :: Text -> JavaGen
raw = tell

indent :: JavaGen -> JavaGen
indent = censor (\text -> T.dropEnd 2 ("  " <> T.replace "\n" "\n  " text)) -- there's probably a better way to do this

packageGen :: Name -> JavaGen
packageGen name = statement $ "package " <> (showt name)

importGen :: Import -> JavaGen
importGen i = statement $ showt i

typeDeclGen :: TypeDecl -> JavaGen
typeDeclGen (ClassTypeDecl c) = classGen c

classHeaderGen :: Class -> JavaGen
classHeaderGen Class{..} = raw $ (accessText classAccess) 
                                 <> emptyIfFalse classIsFinal "final "
                                 <> "class "
                                 <> showt className

fieldGen :: Field -> JavaGen
fieldGen Field{..} = statement $ (accessText fAccess)
                                 <> emptyIfFalse fIsFinal "final "
                                 <> emptyIfFalse fIsStatic "static "
                                 <> showt fType
                                 <> " "
                                 <> showt fName

paramsText :: [Param] -> Text
paramsText params = "(" <>
             ( T.intercalate ", " $ map (\(Param f t n) -> emptyIfFalse f "final " <> showt t <> " " <> showt n) params)
             <> ")"

constructorHeaderGen :: Identifier ->  Constructor -> JavaGen
constructorHeaderGen className (Constructor a p _) = raw $ (accessText a)
                                                           <> showt className
                                                           <> paramsText p

constructorGen :: Identifier -> Constructor -> JavaGen
constructorGen className c@(Constructor a p s) = withBody (constructorHeaderGen className c) $ do
    forM_ s (\(Statement x) -> statement x)

methodGen :: Method -> JavaGen
methodGen Method{..} = do
    let methodHeader = raw $ accessText mAccess
                             <> emptyIfFalse mIsFinal "final "
                             <> emptyIfFalse mIsStatic "static "
                             <> showt mReturnType <> " "
                             <> showt mName
                             <> paramsText mParams
    withBody methodHeader $ forM_ mBody $ \(Statement s) -> statement s


classGen :: Class -> JavaGen
classGen clazz@Class{..} = withBody (classHeaderGen clazz) $ do 
    forM_ classFields fieldGen
    emptyLines 1
    forM_ classConstructors $ \c -> constructorGen className c >> emptyLines 1
    forM_ classMethods $ \m -> methodGen m >> emptyLines 1

accessText :: Access -> Text
accessText Public = "public "
accessText Private = "private "
accessText Package = ""
accessText Protected = "protected "

compilationUnitGen :: CompilationUnit -> JavaGen
compilationUnitGen (CompilationUnit name imports typedecl) = do
    packageGen name
    emptyLines 1
    forM_ imports importGen
    emptyLines 2
    typeDeclGen typedecl

