module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.List

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

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
          <|> parseAtom
          <|> parseString
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
