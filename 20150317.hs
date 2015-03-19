-- My not function
myNot :: Bool -> Bool
myNot True = False
myNot False = True

-- Increment integer by one
incr :: Int -> Int
incr e = (+) e 1

-- Concatenate two lists
sumLists :: [Int] -> [Int] -> [Int]
sumLists lista listb = (++) lista listb

isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'

digits :: String -> [Bool]
digits str
  | (==) (length str) 0 = []
  | (==) (length str) 1 = isDigit (head str) : []
  | otherwise = isDigit (head str) : (digits (tail str))
