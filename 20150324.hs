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

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

{-

Defina funções que retornem:

1. A primeira coordenada de um ponto
2. A segunda coordenada de um ponto
3. Indique se uma reta é vertical ou não

-}

first_ :: Ponto -> Float
first_ p = fst p

second_ :: Ponto -> Float
second_ p = snd p

vertical :: Reta -> Bool
vertical r = first_ (fst r) == second_ (fst r)

-- let r = ((9, 0), (22, 34))

{-

Se uma reta é dada por (y - y1) / (x - x1) = (y2 - y1) / (x2 - x1),
defina uma função que, dada uma cordenada x e uma reta, retorne a coordenada
y tal que o ponto (x, y) faça parte da reta

-}

pontoY :: Float -> Reta -> Float
pontoY x rect = y
  where
    y = snd (snd rect)

type Pessoa = String
type Livro = String

type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [
  ("Rodrigo", "Breakfast for Champions"),
  ("Bruno", "Atlas Shrugged"),
  ("Talita", "Dom Casmurro"),
  ("Rodrigo", "Animal Farm")
  ]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd person = [book | (p, book) <- bd, person == p]

-- livros baseExemplo "Rodrigo" - fuck man, this is great

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd b = [p | (p, book) <- bd, book == b]

-- emprestimos baseExemplo  "Animal Farm"

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado bd l = (snd (head bd) == l) || emprestado (tail bd) l

-- emprestado baseExemplo  "Animal Farm"

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos bd p = length (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p b = []
emprestar bd p b = (++) [(p, b)] bd

-- qtdEmprestimos (emprestar baseExemplo "Rodrigo" "The Fountainhead") "Rodrigo"

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p b = []
devolver bd p b
  | (head bd) == (p, b) = tail bd
  | otherwise = (++) [(head bd)] (devolver (tail bd) p b)

{- Redefina as seguintes funções utilizando compreensão de listas -}

membro :: [Int] -> Int -> Bool
membro xs n = (==) (length [number | number <- xs, number == n]) 1

{- Funções de Processamento de Texto -}

-- returns the first word from a string
getWord :: String -> String
getWord [] = ""
getWord str
  | not (isSpace (head str)) = [(head str)] ++ (getWord (tail str))
  | otherwise = ""

-- helper function
isSpace :: Char -> Bool
isSpace c = c == ' '

-- removes the first word from a string
dropWord :: String -> String
dropWord [] = ""
dropWord str
  | not (isSpace (head str)) = dropWord (tail str)
  | otherwise = str

-- remove all leading space chars from a string
dropSpace :: String -> String
dropSpace [] = ""
dropSpace str
  | isSpace (head str) = dropSpace (tail str)
  | otherwise = str

type Word = String

-- helper function
-- like getWord, but works with string leaded by space chars
forceGetWord :: String -> String
forceGetWord [] = []
forceGetWord str
  | isSpace (head str) = forceGetWord (tail str)
  | otherwise = getWord str

-- space-only-separator split function
splitWords :: String -> [Word]
splitWords [] = []
splitWords str
  | str == getWord str = [(forceGetWord str)]
  | otherwise = [(forceGetWord (dropSpace str_))] ++ (splitWords (dropSpace (dropWord str_)))
  where
    str_ = dropSpace str

-- yeah, exactly what you thought
trim :: String -> String
trim str = dropSpace (reverse (dropSpace (reverse str)))

type Line = [Word]

-- função nomeada getLinee para evitar conflito com funções da lib padrão. Por favor, considere.
getLinee :: Int -> [Word] -> Line
getLinee n ws
  | n == 0 || ws == [] = []
  | len <= n = [(head ws)] ++ (getLinee (n - len) (tail ws))
  | otherwise = []
  where
    len = length (head ws)

-- getLinee 10 ["Oi", "meu", "nome", "é", "Rodrigo"]

dropLine :: Int -> [Word] -> Line
dropLine n ws
  | n == 0 = ws
  | len <= n = (dropLine (n - len) (tail ws))
  | otherwise = ws
  where
    len = length (head ws)

-- dropLine 10 ["Oi", "meu", "nome", "é", "Rodrigo"]

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = [getLinee 10 [head ws]] ++ splitLines (tail ws)

-- splitLines ["rodrigo", "alves", "vieira"]

fill :: String -> [Line]
fill st = splitLines (splitWords st)

-- fill "rodrigo alves vieira"

joinLines :: [Line] -> String
joinLines [] = ""
joinLines lines
  | (length lines) == 1 = head (head lines)
  | otherwise =  head (head lines) ++ " " ++ joinLines(tail lines)

-- joinLines (fill "red hot chili peppers")
