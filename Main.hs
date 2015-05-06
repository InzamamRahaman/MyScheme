module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.List
import Numeric

-- data type to represent abstract syntax tree
data LispVal = Atom String
              |List [LispVal]
              |DottedList [LispVal] LispVal
              |Number Integer
              |String String
              |Bool Bool
              |Character Char
              |Float Double

-- define Show for LispVal
instance Show LispVal where
  show (String str) = str
  show (Bool b) = show b
  show (Number i) = show i
  show (Atom str) = str
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (DottedList xs _) = "(" ++  unwords (map show xs) ++ ")"
  show (Character ch) = show ch
  show (Float d) = show d


-- used to fashion readHex, readOct ,and readBin
readUsingFun fun = fst . head . fun



escapes :: Parser Char
escapes = do
            char '\\'
            ch <- oneOf "\"\\ntr"
            return ch

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapes <|> noneOf "\\\"")
                char '"'
                return $ String x

parseCharacterName :: Parser Char
parseCharacterName = do
                        st <- string "newline"  <|> string "space"
                        return $ case st of
                                    "newline" -> '\n'
                                    "space" -> ' '


parseCharacter :: Parser LispVal
parseCharacter = do
                  char '#'
                  char '/'
                  ch <-   try parseCharacterName <|> anyChar
                  return $ Character ch

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseOctal :: Parser LispVal
parseOctal = do
              digits <- many1 (oneOf "01234567")
              return $ Number $ readUsingFun readOct digits

parseHex :: Parser LispVal
parseHex = do
            digits <- many1 (oneOf "0123456789ABCDEFabcdef")
            return $ Number $ readUsingFun readHex digits

binaryToInt :: String -> Integer
binaryToInt = foldl (\x y -> x * 2 + (read (show y))) 0

parseBinary :: Parser LispVal
parseBinary = do
                digits <- many1 (oneOf "01")
                return $ Number $ binaryToInt digits



parseRadix :: Parser LispVal
parseRadix = do
              char '#'
              ch <- oneOf "bodx"
              case ch of
                'd' -> parseDecimal
                'o' -> parseOctal
                'b' -> parseBinary
                'x' -> parseHex

parseFloat :: Parser LispVal
parseFloat = do
                start <- many1 digit
                dot <- char '.'
                rest <- many1 (digit <|> char 'L')
                let num = rd (start ++ (dot:rest))
                return $ Float num
                where
                  rd = read :: String -> Double

parseNumber :: Parser LispVal
parseNumber = parseRadix <|> parseDecimal

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
          <|> parseAtom
          <|> parseString
          <|> try parseFloat
          <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<>=?@^~_"

spaces :: Parser ()
spaces = skipMany1 space


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No valid interpretation " ++ (show err)
  Right val -> "Valid " ++ (show val)



main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
