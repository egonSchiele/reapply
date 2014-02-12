import Test.Hspec
import ReApply
import Types
import Text.Parsec

io = flip shouldReturn

main = hspec $ do
  describe "Diff" $ do
    it "should parse a diff successfully" $ do
      io True $ do
        contents <- readFile "spec/test.diff"
        case parse diffParser "" contents of
          Left _ -> return False
          Right _ -> return True
