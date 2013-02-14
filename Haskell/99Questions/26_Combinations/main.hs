import Data.List
main = putStrLn $ show output
output = combinations 3 "abcdefg"

combinations n l
    | n > length l = []
    | n == length l = [l]
    | n == 1 = map (:[]) l
combinations n (h:t) = c1 ++ c2
    where
    c1 = map (h:) $ combinations (n - 1) t
    c2 = combinations n t
