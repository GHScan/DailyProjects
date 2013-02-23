import Data.List 
import qualified Data.Set as Set

main = print output
output = length$primeList 10000

primeList maxN = _primeList 2 maxN Set.empty
    where
    _primeList i maxN excl
        | i >= maxN = []
    _primeList i maxN excl = i:_primeList nextp maxN newExcl
        where
        newExcl = foldr Set.insert excl $ takeWhile (<maxN) [i,i+i..]
        nextp = head [x|x<-[i+1..],odd x,not $Set.member x newExcl]

replace v i l = left++v:right
    where (left,_:right) = splitAt i l

-- this is far slower than primeList
primeList2 maxN = map fst$filter (\(i,b)->b) $zip [2..] (drop 2 $_primeList 2 $replicate maxN True)
    where
    _primeList i boolL  
        | i >= maxN = boolL
        | not$boolL !! i = _primeList (i+1) boolL
        | boolL !! i = _primeList (i+1) $foldr (replace False) boolL $takeWhile (<maxN) [i*2,i*3..]
