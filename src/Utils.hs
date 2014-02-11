module Utils where
import Data.Maybe
import System.Directory
import Text.Parsec
import Control.Applicative
import Data.List
import qualified System.IO.Strict as S

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile filename = do
    exists <- doesFileExist filename
    if exists
      then Just <$> S.readFile filename
      else return Nothing

eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _)  = Nothing


(=~) :: String -> Parsec String () a -> Maybe a
str =~ parser = eitherToMaybe $ parse parser "" str

replace :: (a -> Bool) -> a -> [a] -> [a]
replace func elem arr = case findIndex func arr of
                          Nothing -> arr
                          Just index -> [arr !! i | i <- [0..index - 1]] ++ [elem] ++ [arr !! i | i <- [index + 1..(length arr)]]
