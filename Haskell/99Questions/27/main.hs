import Data.List
main = putStrLn $ show output
output = length$groupSet "123456789" [2,2,5]

groupSet l [] = [[]]
groupSet l (h:t) = [x:y|x<-c,y<-groupSet (l \\ x) t]
    where
    c = combinations h l

combinations n l
    | n > length l = []
    | n == length l = [l]
    | n == 1 = map (:[]) l
combinations n (h:t) = c1 ++ c2
    where
    c1 = map (h:) $ combinations (n - 1) t
    c2 = combinations n t
