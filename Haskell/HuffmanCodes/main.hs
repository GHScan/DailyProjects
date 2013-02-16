import Data.List
import Data.Function

main = putStrLn $ show output
output = decode $encode "helloooo iii am scannnn!"

encode s = (output,table)
    where
    freq = map (\x->(length x, head x)) $group $sort s
    table = huffcode freq
    lookup a = fst$head $filter (\(_,c)->c==a) table
    output = concat $ map lookup s

decode ([],table) = ""
decode (s,table) = c:decode (drop (length pre) s, table)
    where
    (pre,c) = head $filter ((flip isPrefixOf s).fst) table

buildHuffTree l@[x] = l
buildHuffTree l = buildHuffTree $(Mid (getNodeWeight h1 + getNodeWeight h2) h1 h2):t
    where (h1:h2:t) = sortBy (compare `on` getNodeWeight) l
setCode s (Mid w l r) = setCode (s++"0") l ++ setCode (s++"1") r
setCode s (Leaf w c) = [(s,c)]
huffcode freq = setCode "" $ head tree
    where 
    tree = buildHuffTree $ map (uncurry Leaf) freq

data Node = Leaf Int Char | Mid Int Node Node deriving (Show)
getNodeWeight (Leaf w _) = w
getNodeWeight (Mid w _ _) = w
