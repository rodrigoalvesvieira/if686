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

-- Smaller of two integers
smallerOfTwo :: Int -> Int -> Int
smallerOfTwo a b
	| a < b  = a
	| otherwise  = b

-- Bigger of two integers
biggerOfTwo :: Int -> Int -> Int
biggerOfTwo a b
	| a > b  = a
	| otherwise  = b

-- Smaller of three integers
smallerOfThree :: Int -> Int -> Int -> Int
smallerOfThree a b c
     | a < (smallerOfTwo b c) = a
	 | otherwise = (smallerOfTwo b c)

-- Bigger of three integers
biggerOfThree :: Int -> Int -> Int -> Int
biggerOfThree a b c
     | a > (biggerOfTwo b c) = a
   | otherwise = (biggerOfTwo b c)

factorial :: Int -> Int
factorial 0  = 1
factorial n  = n * factorial (n-1)

-- Find out if an integer is within a list
contains :: Int -> [Int] -> Bool
contains _ [] = False
contains y (a:x)
	| y == a  = True
	| otherwise = contains y x


count :: [t] -> Int
count [] = 0
count (a:x) = 1 + count x
