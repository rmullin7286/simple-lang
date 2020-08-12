module Main where

import SLC.AST.SL
import SLC.AST.Shared
import SLC.Transpile
import SLC.CodeGen
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Control.Monad

main :: IO ()
main = do
    input <- TIO.readFile "test.sl"
    let result = parse (parseFile $ Name [Identifier "test"]) "test.sl" input
    case result of
        Left err -> print err
        Right success -> do
            let transpiled = transpileModule success
            forM_ transpiled $ \mod -> do
                let gen =  runJavaGen (genFile mod)
                TIO.putStrLn gen


