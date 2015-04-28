module Main where

{- Sétimo trabalho de PLC -}

type Vertex t = t
type Edge t = (Vertex t, Vertex t, Int)
data Graph t = Graph [Vertex t] [Edge t] deriving (Eq, Ord, Show)

-- 1.
compose :: (Eq t) => (t -> t) -> [(t -> t)] -> [(t -> t)]
compose g [] = []
compose g (f:fs) = (g . f) : compose g fs

-- 2.
mapGraph :: (Eq t) => (t -> t) -> Graph t -> Graph t
mapGraph f (Graph v adj) = Graph (map f v) adj

foldGraph :: (Eq t) => (t -> t -> t) -> Graph t -> t
foldGraph f (Graph v adj) = foldr1 f v

-- 3.

main :: IO()
main = do
	putStrLn "Hello World"
