module SLC.Util where

import qualified Data.Text as T
import Data.Char

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t = case T.uncons t of
                        Just (x, xs) -> (toUpper x) `T.cons` xs
                        Nothing      -> ""