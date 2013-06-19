import Text.Parsec
import Text.Parsec.String

-- RWH ch. 16 uses Parsec 2 (module Text.ParserCombinators.Parsec)
-- and annotates the parsers with the following type: GenParser Char st [String]

-- type GenParser tok st = Parsec [tok] st
-- type Parser = Parsec String ()

-- st above is "state" or something, some kind of parser state that can be threaded through
-- in Parser this is just unit, ie ignored

csvFile :: Parser [[String]]
csvFile =
  do result <- many line
     eof
     return result

line :: Parser [String]
line =
  do result <- cells
     eol
     return result

cells :: Parser [String]
cells =
  do first <- cellContent
     next <- remainingCells
     return (first : next)

remainingCells :: Parser [String]
remainingCells =
  (char ',' >> cells)
  <|> (return [])

cellContent :: Parser String
cellContent =
  many (noneOf ",\n")

eol :: Parser Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

