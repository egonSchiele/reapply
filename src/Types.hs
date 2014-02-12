module Types where
import Text.Parsec hiding (Line)
import Control.Applicative
import Text.Printf
import Data.List

join elem list = concat $ intersperse elem list

data Diff = Diff {
          aPath :: String,
          bPath :: String,
          chunks :: [Chunk]
}

instance Show Diff where
    show (Diff a b chunks) = printf "--- %s\n+++ %s\n%s" a b (join "\n" . map show $ chunks)

data Chunk = Chunk {
           startA :: Int,
           startB :: Int,
           sizeA  :: Int,
           sizeB  :: Int,
           changes :: [Line]
}

instance Show Chunk where
    show (Chunk stA stB szA szB ch) = printf "@@ -%d,%d +%d,%d @@\n%s" stA szA stB szB (join "\n" . map show $ ch)

data ModType = Common | Addition | Deletion

instance Show ModType where
  show Common = " "
  show Addition = "+"
  show Deletion = "-"

data Line = Line {
          modType :: ModType,
          contents :: String
}

instance Show Line where
  show (Line m c) = show m ++ c

startAndSize = do
    startA_ <- read <$> many1 digit
    char ','
    sizeA_ <- read <$> many1 digit
    return (startA_, sizeA_)

br = anyChar `manyTill` newline

lineParser = do
    modType_ <- oneOf " -+"
    contents_ <- manyTill anyChar newline
    return $ case modType_ of
               ' ' -> Line Common contents_
               '-' -> Line Deletion contents_
               '+' -> Line Addition contents_

chunkParser = do
    (startA_, sizeA_) <- string "@@ -" >> startAndSize
    (startB_, sizeB_) <- string " +" >> startAndSize
    br
    chunkLines <- many1 lineParser
    -- TODO: parse lines into a vs b's lines:
    return $ Chunk startA_ startB_ sizeA_ sizeB_ chunkLines

diffParser = do
    fileA <- string "---" >> spaces >> manyTill anyChar space
    br
    fileB <- string "+++" >> spaces >> manyTill anyChar space
    br
    chunk <- chunkParser
    return $ Diff fileA fileB [chunk]

main = do
    diff <- readFile "test.diff"
    case parse diffParser "" diff of
      Left err -> print err
      Right obj -> print obj

