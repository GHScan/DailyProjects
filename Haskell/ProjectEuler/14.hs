main = print output

collatzLength n 
    | n == 1 = 1
    | even n = 1 + collatzLength (n `div` 2)
    | otherwise = 1 + collatzLength (1 + n * 3)

output = snd $ maximum $ map (\x->(collatzLength x,x)) [1..1000000]
