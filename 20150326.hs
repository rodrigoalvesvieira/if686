{-

Exercícios de casa

Defina uma função polimórfica que ordena uma lista de valores para os quais os
operadores de comparação ((>), (>=), etc.) estão definidos.
-}

-- my mra2

-- HashTable
type Key = Int
type Value = Int
type HashTable = [(Key, Value)]

database :: HashTable
database = [(21,4),(212,43),(34,336),(14,3422),(2,5),(8,3),(7,1),(12,8),(16,3),(10,2),(2,4),(1,4),(3,6),(4,2),(2,5),(8,3),(7,1),(12,8),(16,3),(10,2)]

get :: HashTable -> Int -> Int
get [] key = -1
get h key
	| hasKey h key == False = -1
	| otherwise = snd (h!!(collision h key 0))

put :: HashTable -> Int -> Int -> HashTable
put h key value
 	| (hasKey h key == False) = (take (collision h 0 0) h) ++ [(key, value)] ++ (drop ((collision h 0 0) + 1) h)
 	| otherwise = (take (collision h key 0) h) ++ [(key, value)] ++ (drop ((collision h key 0) + 1) h)

remove :: HashTable -> Int -> HashTable
remove h key
 	| not (hasKey h key) = error "error"
 	| otherwise = (take (collision h key 0) h) ++ [(0, 0)] ++ (drop ((collision h key 0) + 1) h)

hasKey :: HashTable -> Int -> Bool
hasKey [] _ = False
hasKey (x:xs) key
 	| (fst x == key) = True
 	| otherwise = hasKey xs key

collision :: HashTable -> Int -> Int -> Int -- linear probing
collision h key s
 	| (fst (h!!(p)) == key) = p
 	| otherwise = collision h key (s + 3)
 	where
 		p = (key + s) `mod` (length database)

{-

Funções da aula

-}

takee :: [t] -> Int -> [t]
takee [] _ = []
takee l 0 = []
takee (a:as) n = [a] ++ takee as (n - 1)

-- let li = ["rodrigo", "alves", "vieira", "beatriz", "barbosa"]
-- takee li 3

drope :: [t] -> Int -> [t]
drope [] _ = []
drope l 0 = l
drope (a:as) n = drope as (n - 1)

-- drope li 3

-- isEven :: t -> Bool
-- isEven n = (n `mod` 2) == 0

takeWhilee :: (t -> Bool) -> [t] -> [t]
takeWhilee _ [] = []
takeWhilee pred (a:as)
	| pred a = a : takeWhilee pred as
	| otherwise = []

-- takeWhilee (>6) [7, 8, 9, 10, 1, 2, 3, 4, 5]

dropWhilee :: (t -> Bool) -> [t] -> [t]
dropWhilee _ [] = []
dropWhilee pred (a:as)
	| pred a = dropWhilee pred as
	| otherwise = a : as

-- dropWhilee (>6) [7, 8, 9, 10, 1, 2, 3, 4, 5]

{-

Defina uma função polimórfica que ordena  uma lista de valores para os quais os
operadores de comparação ((>), (>=), etc.) estão definidos.

-}

-- a dummy quicksort for comparable types
psort :: Ord t => [t] -> [t]
psort [] = []
psort (x:xs) =
    let smallerSorted = psort [a | a <- xs, a <= x]
        biggerSorted = psort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- psort "Only dimly aware of a certain unease in the air"

-- counts how many times a value occurs in a list
_countOcurrences :: Ord t => [t] -> t -> Int
_countOcurrences [] _ = 0
_countOcurrences (x:xs) t
	| x == t = 1 + _countOcurrences xs t
	| otherwise = _countOcurrences xs t

countOcurrences :: Ord t => [[t]] -> t -> Int
countOcurrences [] _ = 0
countOcurrences (x:xs) t = (_countOcurrences x t) + (countOcurrences xs t)

-- countOcurrences "rodrigo" 'r'
-- countOcurrences [1, 2, 3, 4, 5, 5] 5

agrupar_seq :: Ord t => [t] -> [(t, Int)]
agrupar_seq as
	| as == [] = []
	| otherwise = [(a, _countOcurrences as a)] ++ agrupar_seq (tail as)
	where
		a = head as

joine :: Ord t => [[t]] -> [t]
joine [] = []
joine as = head as ++ joine (tail as)

element :: Eq t => (t, Int) -> [(t, Int)] -> Bool
element _ [] = False
element e as = (fst (head as)) == (fst e)

-- element ('a', 10) [('a', 11)]
-- element ('a', 10) [('b', 11)]

-- ('a', 10) `element` [('b', 11)]

rmD :: Eq t => [(t, Int)] -> [(t, Int)] -> [(t, Int)]
rmD as original
	| as == [] = []
	| uniq == True = (head as) : rmD (tail as) (original)
 	| otherwise = rmD (tail as) original
	where
		uniq = ((head as) `element` original)

_agrupar :: Ord t => [[t]] -> [(t, Int)]
_agrupar as
 	| as == [] = []
	| otherwise = agrupar_seq (joine as) ++ _agrupar (tail as)

agrupar :: Ord t => [[t]] -> [(t, Int)]
agrupar as = (_agrupar as)

-- agrupar ["Red", "Hot", "Chili", "Peppers"]
-- agrupar [[4,2,4,3,4,4,4,5,4,6], [1,2,3,4,5],[2]]
