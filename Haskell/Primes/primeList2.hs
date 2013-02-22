import Data.List 
import qualified Data.Set as Set

main = print output
output = primeList 100

primeList maxN = _primeList 2 maxN Set.empty
    where
    _primeList i maxN excl
        | i >= maxN = []
    _primeList i maxN excl = i:_primeList nextp maxN newExcl
        where
        newExcl = foldr Set.insert excl $ takeWhile (<maxN) [i,i+i..]
        nextp = head [x|x<-[i+1..],odd x,not $Set.member x newExcl]
