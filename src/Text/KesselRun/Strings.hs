module Text.KesselRun.Strings where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Applicative ((<|>), some)

import Text.KesselRun.Defs

type ParserS = Parser Char

string :: String -> ParserS String
string = exact

char :: Char -> ParserS Char
char = token

digit :: ParserS Char
digit = satisfy isDigit

letter :: ParserS Char
letter = satisfy isAlpha

alphaNum :: ParserS Char
alphaNum = satisfy isAlphaNum

space :: ParserS ()
space = satisfy isSpace >> return ()

spaces :: ParserS ()
spaces = many space >> return ()

int :: ParserS Int
int = do
  sgn <- string "-" <|> return []
  n <- some digit
  return $ read (sgn ++ n)
