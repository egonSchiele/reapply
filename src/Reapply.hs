module ReApply where
import qualified Types as T
import Control.Applicative
import Data.Maybe
import Utils
import Text.Parsec
import Control.Monad
import Data.List

-- logFile = ".reapply"

-- apply :: T.Item -> IO ()
-- apply item = do
--     items <- liftM ((=<<) readItemList) $ maybeReadFile logFile
--     let oldItem  = find ((== T.path item) . T.path) <$> items
--         newItems = replace ((== T.path item) . T.path) item <$> items
--         content  = show $ fromMaybe [item] newItems
--     writeFile logFile content
