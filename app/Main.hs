module Main where

import Text.KesselRun

thing :: Parser Char String
thing = exact "hello" >> exact "world"

main :: IO ()
main = putStrLn $ show $ unsafeParse thing "helloworld"
