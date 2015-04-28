module Main where

import Data.List (sort)
import Data.Char

{- Oitavo trabalho de PLC -}

aux :: (Ord t, Num t) => [t] -> [t] -> [[t]]
aux a [] = [a]
aux a (h:t) = [[x | x <- a, x <= h]] ++ (aux [x | x <- a, x > h] t)

divi :: (Ord t, Num t) => [t] -> [t] -> [[t]]
divi x y = [a | a <- (aux y x), a /= []]

listPartitioner :: (Ord t, Num t) => [t] -> ([t] -> [[t]])
listPartitioner list = \x -> (divi (sort list) (sort x))

{- Exercícios do slide 05_FUNCOES_ALTA_ORDEM -}

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto s = map ord s

myMap :: (x -> y) -> [x] -> [y]
myMap f l = [f x | x <- l]

main :: IO()
main = do
	putStrLn ":-B"
	putStrLn $ show $ map sqrt [1, 2, 3, 4]
	putStrLn $ show $ posicaoAlfabeto "abcxyz"
	putStrLn $ show $ myMap sqrt [1, 2, 3, 4]
