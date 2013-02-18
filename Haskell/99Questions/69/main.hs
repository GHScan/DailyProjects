import Data.List hiding (insert)

main = putStrLn $ show output
output = (t ==)$ ds2tree$tree2ds t
    where 
    t = fromList "jfksdlabs"

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show, Eq)

insert :: (Ord a, Eq a) => a->Node a->Node a
insert v Empty = Branch v Empty Empty
insert v t@(Branch a l r)
    | v == a = t
    | v < a = Branch a (insert v l) r
    | v > a = Branch a l (insert v r)

fromList l = foldr insert Empty l

tree2ds Empty = "."
tree2ds (Branch a l r) = [a] ++ tree2ds l ++ tree2ds r

ds2tree s = fst$_ds2tree s
    where
    _ds2tree ('.':t) = (Empty, t)
    _ds2tree (h:t) = (Branch h left right,t3)
        where
        (left,t2) = _ds2tree t
        (right,t3) = _ds2tree t2
