import Text.Parsec
import Text.Parsec.String
import Test.HUnit

-- RWH ch. 16 uses Parsec 2 (module Text.ParserCombinators.Parsec)
-- and annotates the parsers with the following type: GenParser Char st [String]

-- type GenParser tok st = Parsec [tok] st
-- type Parser = Parsec String ()

-- st above is "state" or something, some kind of parser state that can be threaded through
-- in Parser this is just unit, ie ignored

csvFile :: Parser [[String]]
csvFile = endBy line eol

line :: Parser [String]
line = sepBy cell (char ',')

cell :: Parser String
cell = many (noneOf ",\n")

eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

unsafe input =
  case parseCSV input of
    Left e -> error (show e)
    Right res -> res

testEmpty = TestCase (assertEqual "empty input" [] (unsafe ""))
testOne = TestCase (assertEqual "single line, single col" [["foo"]] (unsafe "foo\n"))
testFew = TestCase (assertEqual "single line, multiple cols" [["foo", "bar"]]
                    (unsafe "foo,bar\n"))
testLines = TestCase (assertEqual "multiple lines" [["foo", "bar"],["baz", "qux"]]
                    (unsafe "foo,bar\nbaz,qux\n"))

-- this test breaks when run with naive `eol` parser. succeeds with `option` as above
broken = TestCase (assertEqual "broken test" [["foo", "bar"],["baz", "qux"]]
                    (unsafe "foo,bar\n\rbaz,qux\n\r"))

main = do runTestTT (TestList [testEmpty, testOne, testFew, testLines, broken])