module Utils
(
 primes,
 primeFactors,
 factorCount,
 ) where

import Data.List

primes = queue [2..]
    where queue (x:xs) = x:queue [v|v<-xs,v `mod` x /= 0]

primeFactors = iterate primes
    where iterate primes@(p:rest) n
            | n == 1 = []
            | n `mod` p == 0 = p:iterate primes (n `div` p)
            | otherwise = iterate rest n

factorCount =  product. map ((+ 1). length) . group . primeFactors
