main = print output

solve n [] = 0
solve n l@(x:rest) 
    | n < x = 0
    | n == x = 1
    | otherwise = solve n rest + solve (n - x) l

coins = [1,2,5,10,20,50,100,200]

output = solve 200 coins
