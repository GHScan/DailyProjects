import Data.List
main = putStrLn $ show output
output = length$permutation2 "123456789"

permutation l@[x] = [l]
permutation l@(h:t) = [_insert i sub|i<-[0..length l - 1],sub<-permutation t]
    where 
    _insert i sub = left ++ h:right
        where
        (left,right) = splitAt i sub

permutation2 l@[x] = [l]
permutation2 l = concat [_use i|i<-[1..length l]]
    where 
    _use i = map (_head:) $permutation2 _tail
        where
        _head = l !! (i - 1)
        _tail = (init $take i l) ++ drop i l
