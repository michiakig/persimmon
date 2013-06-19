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
cell = quotedCell <|> many (noneOf ",\n")

quotedCell :: Parser String
quotedCell =
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at end of cell"
     return content

quotedChar :: Parser Char
quotedChar =
      noneOf "\""
      -- this instance of try is highlighted as being important, to correctly parse the closing quote
      -- also notable for occuring to the right of <|>, or to the left of some internal <|> in string?
  <|> try (string "\"\"" >> return '"')

eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

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

testQuoted = TestCase (assertEqual "quoted cell" [["foo,bar", "baz"]]
                       (unsafe "\"foo,bar\",baz\n"))

testLarger = TestCase (assertEqual "larger" [["Product", "Price"],
                                             ["O'Reilly Socks", "10"],
                                             ["Shirt with \"Haskell\" text", "20"],
                                             ["Shirt, \"O'Reilly\" version", "20"],
                                             ["Haskell Caps","15"]]
                       (unsafe "\"Product\",\"Price\"\n\"O'Reilly Socks\",10\n\"Shirt with \"\"Haskell\"\" text\",20\n\"Shirt, \"\"O'Reilly\"\" version\",20\n\"Haskell Caps\",15\n"))

main = do runTestTT (TestList [testEmpty, testOne, testFew, testLines, broken, testQuoted, testLarger])