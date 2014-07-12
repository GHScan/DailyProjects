
main = print output

fibs = f 1 2
    where f a b = a:f b (a + b)
 
output = sum$ filter ((== 0) . (`mod` 2)) $takeWhile (< 4000000) fibs
