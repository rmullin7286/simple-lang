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

pathToModuleName :: T.Text -> Name
pathToModuleName = Name . map Identifier . T.splitOn "/"