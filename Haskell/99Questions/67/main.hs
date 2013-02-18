import Data.Char

main = putStrLn $ show output
output = t == (string2tree $tree2string t)
    where
    t = fromList "fjadsbfgjkdsaf"

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show,Eq)

insert v Empty = Branch v Empty Empty
insert v t@(Branch a l r)
    | v == a = t
    | v < a = Branch a (insert v l) r
    | v > a = Branch a l (insert v r)

fromList l = foldr insert Empty l

toList Empty = []
toList (Branch a l r) = toList l ++ [a] ++ toList r

tree2string Empty = []
tree2string (Branch a Empty Empty) = [a]
tree2string (Branch a l r) = [a,'('] ++ tree2string l  ++ [','] ++ tree2string r ++ [')']

string2tree l = fst$ _string2tree l
_string2tree [] = (Empty,[])
_string2tree l@(h:t)
    | isPunctuation h = (Empty,l)
_string2tree (h:h2:t)  
    | h2 /= '(' = (Branch h Empty Empty, h2:t)
_string2tree (h:t) = (Branch h left right, tail t3)
    where
    (left,t2) = _string2tree $ tail t
    (right,t3) = _string2tree $ tail t2
