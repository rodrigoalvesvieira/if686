-- Quicksort

bigOfTwo :: Int -> Int -> Int
bigOfTwo a b
	| a > b  = a
	| otherwise  = b

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- Sum digits
sumd 0 = 0
sumd x = (x `mod` 10) + sumd (x `div` 10)

ord :: [Int] -> [Int]
ord [] = []
ord [x] = [sumd x]
ord xs@(x:xt) = [sumd x] ++ ord(xt)

ordenar :: [Int] -> [Int]
ordenar xs = quicksort (ord xs)

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

getEvens :: [Int] -> [Int]
getEvens [] = []
getEvens (a:x)
 | mod a 2 == 0 = a:getEvens x
 | otherwise = getEvens x

fiblist :: Int -> [Int]
fiblist a = [fibonacci a] ++ fiblist(a-1)

{-


Learning Material: not from class

-}

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

search :: Int -> Int -> Bool
search a i
 | i * i > a = True
 | mod a i == 0 = False
 | otherwise = search a (i+1)

isPrime :: Int -> Bool
isPrime a
 | a <= 1 = False
 | otherwise = search a 2

equals :: [Int] -> [Int] -> Bool
equals [] [] = True
equals _ [] = False
equals [] _ = False
equals (h:x) (g:y)
 | h == g = equals x y
 | otherwise = False

countOcurrences :: Int -> [Int] -> Int
countOcurrences _ [] = 0
countOcurrences y (a:x)
	| y == a  = 1 + countOcurrences y x
	| otherwise = countOcurrences y x

bigger :: [Int] -> Int
bigger (a:x)
 | count (a:x) == 1  = a
 | otherwise  = biggerOfTwo a (bigger x)
