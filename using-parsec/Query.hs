import Text.Parsec
import Text.Parsec.String
import Test.HUnit
import Numeric(readHex)

-- this section of ch 16 uses CharParser, which afaict is the same as Parser, ie parser of token stream consisting of chars

p_query :: Parser [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: Parser (String, Maybe String)
p_pair =
  do name <- many1 p_char
     value <- optionMaybe (char '=' >> many p_char)
     return (name, value)

p_char :: Parser Char
p_char = oneOf urlBaseChars
         <|> (char '+' >> return ' ')
         <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: Parser Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d
