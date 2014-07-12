main = print output

pow b e 
    | e == 0 = 1
    | even e = sqr$ pow b (e `div ` 2)
    | otherwise = b * pow b (e - 1)
    where sqr x = x * x

output = sum $ map (read. (:[])) $ show$ pow 2 1000
