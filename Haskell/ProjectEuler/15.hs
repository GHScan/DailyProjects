import qualified Data.Map as Map

main = print $ output 20

search cache x y n 
    | Map.member (x,y) cache = (cache Map.! (x,y), cache)
    | x == n = (1, Map.insert (x,y) (n - y) cache)
    | y == n = (1, Map.insert (x,y) (n - x) cache)
    | otherwise = 
        let (lv,cache1) = search cache (x + 1) y n
            (rv,cache2) = search cache1 x (y + 1) n
            v = lv + rv
        in (v, Map.insert (x,y) v cache2)

output n = fst $ search Map.empty 0 0 n
