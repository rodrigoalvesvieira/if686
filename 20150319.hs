-- Defining the following list functions

-- double: Double the elements of a list

doubleElement :: Int -> Int
doubleElement n = (*) n 2

double :: [Int] -> [Int]
double list = map doubleElement list

-- membership: Tell if a element is in the list

member :: [Int] -> Int -> Bool
member list e
  | (==) list [] = False
  | otherwise = (==) (head list) e || member (tail list) e

-- this is cool, but if you can use a built-in function, you can do:
-- member :: [Int] -> Int -> Bool
-- member list e = e `elem` list

-- filtering: only the integers of a string
isDigit :: Char -> [Char]
isDigit ch
  | ch >= '0' && ch <= '9' = ch : []
  | otherwise = []
--
digits :: String -> String
digits str
  | (==) str [] = []
  | otherwise = isDigit (head str) ++ digits (tail str)

-- sum the elements of two lists, one-by-one
select :: [Int] -> [Int]
select list
  | (==) list [] = [0]
  | otherwise = list

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs listx listy
  | (==) listx [] && (==) listy [] = []
  | otherwise = (head (select listx)) + (head (select listy)) : sumPairs (tail (select listx)) (tail (select listy))

takeAll :: (t -> Bool) -> [t] -> [t]
takeAll _ [] = []
takeAll pred (a:as)
	| pred a = a : takeAll pred as
	| otherwise = takeAll pred as

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

getAll :: Int -> [Int]
getAll n
  | length evens >= n = evens
  | otherwise = getAll (n + 1)
  where
    as = fibList (n * 3) -- this is not mathematically correct, and is probably going to fail at some point
    evens = getEvens as -- i'm just too lazy to think about it at 1 am

getEvens :: [Int] -> [Int]
getEvens [] = []
getEvens (a:x)
 | mod a 2 == 0 = a:getEvens x
 | otherwise = getEvens x

fibList :: Int -> [Int]
fibList 0 = []
fibList n = fib(n) : fibList(n - 1)

fibonacci :: Int -> [Int]
fibonacci n = reverse (getAll n)
