import Data.List
import Data.Char

main = putStrLn $ show output
output = split2Prime 600851475143 []

split2Prime 1 l = l
split2Prime n l = split2Prime (n `div` p) (p:l)
    where
    p = head$ filter ((==0).mod n) primeList

primeList = 2:_primeList [2]
    where
    _primeList l = nextP:_primeList (l++[nextP])
        where
        isPrime n = all ((/=0).mod n) $ takeWhile (<=floor (sqrt $ fromIntegral n)) l
        nextP = head [x|x<-[last l+1..],odd x,isPrime x]
