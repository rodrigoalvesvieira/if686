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
