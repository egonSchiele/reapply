module Types where
import Utils
import Text.Parsec

data Item = Item {
          path :: String,
          content :: String
} deriving (Eq)

data Diff = Diff {
          aPath :: String,
          bPath :: String,
          chunks :: [Chunk]
} deriving (Show)

data Chunk = Chunk {
           startA :: Int,
           startB :: Int,
           sizeA  :: Int,
           sizeB  :: Int,
           linesA :: [Line],
           linesB :: [Line]
} deriving (Show)

data Modtype = Insertion | Deletion deriving (Show)

data Line = Line {
          modType :: ModType
          data :: String
} deriving (Show)

tmp $ diff -u a b
--- a	2014-02-11 15:07:26.000000000 -0800
+++ b	2014-02-11 15:08:07.000000000 -0800
@@ -1,2 +1,4 @@
 hello
-there
+
+world
+ok

instance Show Item where
  show item = show (path item, content item)
