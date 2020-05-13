module Main where

import System.Environment
import SimpleLang.Lang
import SimpleLang.Transpile
import SimpleLang.CodeGen
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec
import Control.Monad
import TextShow

main :: IO ()
main = do
    [path] <- getArgs
    contents <- TIO.readFile path
    case parse (parseModule (Name [Identifier "foo", Identifier "bar", Identifier "baz"])) path contents of
        Left err -> putStrLn $ errorBundlePretty err
        Right good -> do
            let java = transpile good
            let output = map (runJavaGen . compilationUnitGen) java
            forM_ (zip [1..] output) $ \(i,x) -> do
                TIO.writeFile ("test" <> show i <> ".java") x


