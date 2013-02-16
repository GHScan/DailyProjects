import Data.List
import Data.Function

main = putStrLn $ show output
output = huffcode [(45,'a'),(13,'b'),(12,'c'),(16,'d'),(9,'e'),(5,'f')]

buildHuffTree l@[x] = l
buildHuffTree l = buildHuffTree $(Mid (getNodeWeight h1 + getNodeWeight h2) h1 h2):t
    where (h1:h2:t) = sortBy (compare `on` getNodeWeight) l
setCode s (Mid w l r) = setCode (s++"0") l ++ setCode (s++"1") r
setCode s (Leaf w c) = [[(s,c)]]
huffcode freq = setCode "" $ head tree
    where 
    tree = buildHuffTree $ map (uncurry Leaf) freq

data Node = Leaf Int Char | Mid Int Node Node deriving (Show)
getNodeWeight (Leaf w _) = w
getNodeWeight (Mid w _ _) = w
