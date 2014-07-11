
main = print output

output = take 10 $ primes

qsort [] = []
qsort (x:xs) = qsort [v|v<-xs,v<=x] ++ [x] ++ qsort [v|v<-xs,v>x]

qsort2 l@[] = l
qsort2 l@[x] = l
qsort2 (x:xs) = qsort2 [v|v<-xs2,v<x2] ++ x2:[v|v<-xs2,v==x2] ++ qsort2 [v|v<-xs2,v>x2]
    where (left, x2:right) = splitAt ((length xs) `div` 2)  xs
          xs2 = left ++ x:right

permutation [] = [[]]
permutation (x:xs) = [insert i x rest|rest<-permutation xs,i<-[0..length xs]]
    where
        insert 0 x l = x:l
        insert i x (x2:xs2) = x2:insert (i - 1) x xs2

combination 0 l = [[]]
combination n [] = []
combination n (x:xs) = [x:rest|rest<-combination (n - 1) xs] ++ combination n xs

primes = queue [2..]
    where queue (x:xs) = x:queue [v|v<-xs,v`mod`x/=0]
