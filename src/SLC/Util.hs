{-
Module      : SLC.Util
Description : Utility functions that wouldn't fit in other modules
Copyright   : (c) Ryan Mullin, 2020
License     : GPL-3
Maintainer  : ryan.mullin12@gmail.com
Stability   : experimental
Portability : portable
-}
module SLC.Util where

import qualified Data.Text as T
import SLC.AST.Shared
import Data.Either

pathToModuleName :: T.Text -> Name
pathToModuleName = Name . map Identifier . T.splitOn "/"

partitionMap :: (a -> Either b c) -> [a] -> ([b], [c])
partitionMap f = partitionEithers . map f

replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 x (_:xs) = x:xs
replaceAt n x (y:xs) = y : replaceAt (n - 1) x xs
replaceAt _ _ [] = error "out of bounds"

removeAt :: Int -> [a] -> [a]
removeAt 0 (_:xs) = xs
removeAt _ [] = []
removeAt n (x:xs) = x : removeAt (n - 1) xs

empty :: [a] -> Bool
empty [] = True
empty _ = False