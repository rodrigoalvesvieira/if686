-- Defining the following list functions

-- double: Double the elements of a list

doubleElement :: Int -> Int
doubleElement n = (*) n 2

double :: [Int] -> [Int]
double list = map doubleElement list

-- membership: Tell if a element is in the list

member :: [Int] -> Int -> Bool
member list e
  | (==) (length list) 0 = False
  | otherwise = (==) (head list) e || member (tail list) e

-- this is cool, but if you can use a built-in function, you can do:
-- member :: [Int] -> Int -> Bool
-- member list e = e `elem` list

-- filtragem: only the integers of a string
isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'

digits :: String -> [Bool]
digits str
  | (==) (length str) 0 = []
  | (==) (length str) 1 = isDigit (head str) : []
  | otherwise = isDigit (head str) : (digits (tail str))

-- sum the elements of two lists

-- sumPairs :: [Int] -> [Int] -> [Int]
-- sumPairs lista list b =
