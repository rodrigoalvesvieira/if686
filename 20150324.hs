{-

Funções do trabalho

-}

-- Mergesort

-- recebe duas listas ordenadas e retorna uma lista nova
-- que é o agrupamento das duas, ordenadas
merge []         ys                   = ys
merge xs         []                   = xs
merge xs@(x:xt) ys@(y:yt) | x <= y    = x : merge xt ys
                          | otherwise = y : merge xs yt

-- decompôe recursivamente a lista
split :: [Int] -> ([Int], [Int])
split list
  | (==) list [] = ([], [])
  | (==) (length list) 1 = (list, [])
  | otherwise = (x: xs, y: ys)
  where
    (xs, ys) = split (tail (tail list))
    x = head list
    y = head (tail list)

-- decompôe, ordena e reagrupa a lista
mergesort :: [Int] -> [Int]
mergesort [x] = [x]
mergesort xs  = let (as, bs) = split xs
                in merge (mergesort as) (mergesort bs)


{-

Funções da aula

-}

-- 1. Defina a função *menorMaior* que reebe três inteiros e retorna uma tupla com o menor e o maior deles.
menorDeDois :: Int -> Int -> Int
menorDeDois a b
	| a < b  = a
	| otherwise  = b

maiorDeDois :: Int -> Int -> Int
maiorDeDois a b
	| a > b  = a
	| otherwise  = b

menorDeTres :: Int -> Int -> Int -> Int
menorDeTres a b c
     | a < (menorDeDois b c) = a
	 | otherwise = (menorDeDois b c)

maiorDeTres :: Int -> Int -> Int -> Int
maiorDeTres a b c
      | a > (maiorDeDois b c) = a
 	 | otherwise = (maiorDeDois b c)

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (menor, maior)
  where
    menor = menorDeTres a b c
    maior = maiorDeTres a b c

-- 2. Defina a função *ordenaTripla* que recebe uma tripla de inteiros e ordna a mesma

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (x, y, z)
  where
    list = mergesort [a, b, c]
    x = list!!0
    y = list!!1
    z = list!!2
