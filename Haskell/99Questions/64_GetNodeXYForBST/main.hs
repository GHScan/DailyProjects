
main = putStrLn $ show output
output = getPosition (Branch 3 (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 5 Empty Empty))

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show)

getPosition t = map (\(x,(a,y))->(a,(x,y))) $zip [1..] $getDepth t 1
    where 
    getDepth Empty d = []
    getDepth t@(Branch a l r) d = getDepth l (d + 1) ++ [(a,d)] ++ getDepth r (d + 1)
