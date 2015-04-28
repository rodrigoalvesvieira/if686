module Main where

{- Sexto trabalho de PLC -}

-- 1.
data Graph t = Nil | Graph [[(t, t)]] deriving (Eq, Show)

-- 2.

main :: IO()
main = do
	putStrLn "I am Heisenberg!"
