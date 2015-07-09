-- TRABALHO 11

-- 1.
import Data.Char

type Value = String
type Hash = [Value]
type Key = String
type Code = Int

get :: Hash -> Code -> Maybe Value
get hash Code
	| Code >= (length hash) = Nothing
	| otherwise = Just (hash!!Code)

rem :: Hash -> Code -> Maybe Hash
rem hash Code
	| Code >= (length hash)  = Nothing
	| otherwise = Just ( pvt_put hash (Code, ""))

pvt_put :: Hash -> (Code, Value) -> Hash
pvt_put hash (Code, value)
	| Code >= (length hash) = pvt_put (exp hash (Code - (length hash) + 300)) (Code,value)
	| otherwise =  (take Code hash) ++ (value:[])++(drop (Code + 1) hash )

exp :: Hash -> Int -> Hash
exp hash 0 = hash
exp hash len = exp (hash ++ ("":[])) (len - 1)

getEval :: Key -> Code
getEval str = auxEval str 0 databases

auxEval :: String -> Int -> Int -> Int
auxEval "" sum p  = sum
auxEval (h:t) sum p = auxEval t (mod ((mod (27 * sum) p) + getVal(h)) p) p

getVal :: Char -> Int
getVal ch = fromEnum(ch) - fromEnum('a') + 1

databases :: Int
databases = 47

-- 2.

strtok :: String -> String -> [String] -> [String]
strtok [] acc ret = [a | a <-  [acc] ++ret, (length a) /= 0]
strtok (h:t) acc ret
	| h == ' ' = strtok t "" (ret ++ [acc])
	| otherwise = strtok t (acc ++ [h]) ret

done :: String -> Bool
done [] = True
done (h:t) = (((h >= 'a') && (h <= 'z')) || ( (h>= 'A') && (h <= 'Z')) || (h == ' ')) && (done t)

up :: String -> String
up [] =[]
up (h:t) = [toUpper h]++(up t)


process :: String -> Maybe [String]
process a
	| not (done a) = Nothing
	| otherwise = Just (strtok (up a) "" [])


print :: Maybe [String] -> IO ()
print Nothing = putStr ""
print (Just []) = putStr ""
print (Just (h:t) ) = do { putStrLn h; print (Just t)}

main :: IO ()
main = do {
	putStrLn "Entre com a String:";
	l <- getLine;
	lp <- return (process l);
	print lp;
	main;
}
