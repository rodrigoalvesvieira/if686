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
  