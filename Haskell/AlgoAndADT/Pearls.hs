import Data.List

main = print output

output = length$queens 8

qsort [] = []
qsort (x:xs) = qsort [v|v<-xs,v<=x] ++ [x] ++ qsort [v|v<-xs,v>x]

qsort2 l@[] = l
qsort2 l@[x] = l
qsort2 (x:xs) = qsort2 [v|v<-xs2,v<x2] ++ x2:[v|v<-xs2,v==x2] ++ qsort2 [v|v<-xs2,v>x2]
    where (left, x2:right) = splitAt ((length xs) `div` 2)  xs
          xs2 = left ++ x:right

permutation [] = [[]]
permutation (x:xs) = [insert i x rest|rest<-permutation xs,i<-[0..length xs]]
    where insert 0 x l = x:l
          insert i x (x2:xs2) = x2:insert (i - 1) x xs2

combination 0 l = [[]]
combination n [] = []
combination n (x:xs) = [x:rest|rest<-combination (n - 1) xs] ++ combination n xs

primes = queue [2..]
    where queue (x:xs) = x:queue [v|v<-xs,v`mod`x/=0]

queens n = generate n
    where generate 0 = [[]]
          generate y = [(x,y):rest|rest<-generate (y - 1),x<-[1..n],valid (x,y) rest]
          valid (x,y) rest = and [x/=rx && (abs (x-rx) /= y-ry)|(rx,ry)<-rest]

rpnEval source = (foldl eval [] $ words source) !! 0
    where eval (y:x:stack) "+" = (x+y):stack
          eval (y:x:stack) "-" = (x-y):stack
          eval (y:x:stack) "*" = (x*y):stack
          eval (y:x:stack) "/" = (x `div` y):stack
          eval stack numStr = (read numStr):stack

rleEncode :: (Eq a) => [a] -> [(a,Int)]
rleEncode = map (\(x:xs)->(x,1+length xs)) . group 
rleDecode = concat . map (\(x,c)->replicate c x)
