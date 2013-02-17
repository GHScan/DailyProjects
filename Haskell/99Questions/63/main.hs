
main = putStrLn $ show output
output = [isCompleteTree $ buildCompleteTree x|x<-[1..32]]

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show)

buildCompleteTree n = build n 1
    where 
    build n x 
        | x > n = Empty
        | otherwise = Branch x (build n (x * 2)) (build n (x * 2 + 1))
-- Couldn't access the a
isCompleteTree t = size t == maxA t 1
    where
    maxA Empty a = 0
    maxA (Branch _ l r) a = max a $ max (maxA l (a * 2)) (maxA r (a * 2 + 1))
    size Empty = 0
    size (Branch _ l r) = size l + size r + 1
