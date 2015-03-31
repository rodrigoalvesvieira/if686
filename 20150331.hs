-- 2. Implemente a função somatorioHexadecimal. Essa função recebe uma lista de Strings onde cada elemento representa um
-- numero na base hexadecimal e retorna uma String contendo o resultado em hexadecimal do somatório da primeira lista.

hexChar :: Char -> Integer
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

hexToDecimal :: String -> Integer
hexToDecimal [] = 0
hexToDecimal hxStr = (hexChar (last (hxStr) )) + (16 * hexToDecimal (init (hxStr)))

somatorioHexadecimal :: [String] -> String
somatorioHexadecimal str
  | str == [] = []
  | otherwise = "a"

-- 4. Definidos os tipos Vector e Matrix como segue, crie a função multiplicaMatrizes que executa apenas a multiplicação
-- entre duas matrizes quadradas. O retorno da função deve ser uma Matrix que contém o resultado da multiplicação entre os
-- parâmetros. Eficiência não é uma preocupação para resolver essa questão.

type Vector = [Double]
type Matrix = [Vector]

multiplicaMatrizes :: Matrix -> Matrix
multiplicaMatrizes [] = []
