module ReApply where
import qualified Types as T
import Control.Applicative
import Data.Maybe
import Utils
import Text.Parsec
import Control.Monad
import Data.List

readItem :: String -> Maybe T.Item
readItem str = str =~ itemParser

readItemList :: String -> Maybe [T.Item]
readItemList str = str =~ itemListParser

itemListParser = do
    char '['
    items <- itemParser `sepBy` (char ',')
    char ']'
    return items

itemParser = do
    char '('
    path_ <- many1 anyChar
    char ','
    content_ <- many1 anyChar
    char ')'
    return $ T.Item path_ content_

logFile = ".reapply"

apply :: T.Item -> IO ()
apply item = do
    items <- liftM ((=<<) readItemList) $ maybeReadFile logFile
    let oldItem  = find ((== T.path item) . T.path) <$> items
        newItems = replace ((== T.path item) . T.path) item <$> items
        content  = show $ fromMaybe [item] newItems
    writeFile logFile content
