module SimpleLang.Util
       ( globSlFiles
       , moduleNameToPath
       , pathToModuleName
       , capitalizeFirst
       , emptyIfFalse
       ) where

import System.FilePath
import Data.Text(Text)
import qualified Data.Text as T
import SimpleLang.Lang
import System.Directory
import Control.Monad.Extra
import Control.Monad
import Data.Char

moduleNameToPath :: Name -> FilePath
moduleNameToPath (Name idents) = (joinPath $ map show idents)

pathToModuleName :: FilePath -> Name
pathToModuleName = Name . map Identifier . map T.pack . splitPath . dropExtension

globSlFiles :: FilePath -> IO [FilePath]
globSlFiles root = do
    (files, dirs) <- listDirectory root >>= partitionM doesFileExist
    sub <- concatMapM (\dir -> globSlFiles (root </> dir)) dirs
    let thisDir = map (root </>) $ filter ((== "sl") . takeExtension) files
    return $ thisDir <> sub

capitalizeFirst :: Text -> Text
capitalizeFirst t = case T.uncons t of
                        Just (x, xs) -> (toUpper x) `T.cons` xs
                        Nothing      -> ""

emptyIfFalse :: Monoid a => Bool -> a -> a
emptyIfFalse False _ = mempty
emptyIfFalse True m = m





