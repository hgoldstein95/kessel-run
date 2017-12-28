module Text.KesselRun.Defs where

import Control.Monad
import Control.Applicative as App

import Data.Monoid ((<>))

newtype Parser chr a = Parser { parse :: [chr] -> [(a, [chr])] }

data Result chr a = Parsed a
                  | Ambiguous [a]
                  | InputRemaining
                  | Failed
                  deriving (Show)

fromParsed :: Result chr a -> a
fromParsed (Parsed x) = x
fromParsed (Ambiguous _) = error "fromParsed failed: Ambiguous result"
fromParsed (InputRemaining) =
  error "fromParsed failed: Input remaining to be parsed"
fromParsed (Failed) = error "fromParsed failed: No valid parse"

runParser :: Parser chr a -> [chr] -> Result chr a
runParser p s =
  let res = parse p s
  in case filter (null . snd) res of
       [(x, [])] -> Parsed x
       [] -> if null res then Failed else InputRemaining
       xs -> Ambiguous $ map fst xs

unsafeParse :: Parser chr a -> [chr] -> a
unsafeParse p s = fromParsed $ runParser p s

someParse :: Parser chr a -> [chr] -> Maybe a
someParse p s =
  case runParser p s of
    Parsed x -> Just x
    Ambiguous xs -> Just $ head xs
    _ -> Nothing

instance Functor (Parser chr) where
  fmap f p = Parser $ \s -> [(f a, b) | (a, b) <- parse p s]

instance Applicative (Parser chr) where
  pure x = Parser $ \s -> [(x, s)]
  pf <*> px = Parser $ \s ->
    [(f a, s'') | (f, s') <- parse pf s, (a, s'') <- parse px s']

instance Monad (Parser chr) where
  return = pure
  p >>= f = Parser $ \s -> do
    (a, s') <- parse p s
    parse (f a) s'

instance MonadPlus (Parser chr) where
  mzero = Parser $ const []
  mplus p q = Parser $ \s -> parse p s ++ parse q s

instance Alternative (Parser chr) where
  empty = mzero
  (<|>) = option

instance Monoid a => Monoid (Parser chr a) where
  mempty = return mempty
  mappend p q = Parser $ \s ->
    [(a <> b, s'') | (a, s') <- parse p s, (b, s'') <- parse q s']

option :: Parser chr a -> Parser chr a -> Parser chr a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

next :: Parser a a
next = Parser $ \s ->
  case s of
    [] -> []
    (x : xs) -> [(x, xs)]

satisfy :: (chr -> Bool) -> Parser chr chr
satisfy f = do { c <- next; if f c then return c else empty }

token :: Eq chr => chr -> Parser chr chr
token c = satisfy (== c)

exact :: Eq chr => [chr] -> Parser chr [chr]
exact [] = return []
exact (x : xs) = do { _ <- token x; _ <- exact xs; return (x : xs) }

many1 :: Parser chr a -> Parser chr [a]
many1 = App.some

many :: Parser chr a -> Parser chr [a]
many = App.many
