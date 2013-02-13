
main = putStrLn $ show output
output = length $quickSort2 [10000,9999..1]

quickSort [] = []
quickSort (h:t) = quickSort [x|x<-t,x<=h] ++ [h] ++ quickSort [x|x<-t,x>h]

quickSort2 l@[] = l
quickSort2 l@[a] = l
quickSort2 l@[a,b] = if a < b then l else [b,a]
quickSort2 l = 
        quickSort2 [x|x<-t,x<=h] ++ [h] ++ quickSort2 [x|x<-t,x>h]
    where 
    (lh:lt, rh:rt) = splitAt ((length l) `div` 2) l
    (h:t) = rh:lt ++ lh:rt
