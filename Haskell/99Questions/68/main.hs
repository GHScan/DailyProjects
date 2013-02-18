import Data.List hiding (insert)

main = putStrLn $ show output
output = (t ==) $buildFromPreIn pl il
    where 
    t = fromList "abjkdfsjkae"
    (pl, il) = (preorder t, inorder t)

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show, Eq)

insert :: (Ord a, Eq a) => a->Node a->Node a
insert v Empty = Branch v Empty Empty
insert v t@(Branch a l r)
    | v == a = t
    | v < a = Branch a (insert v l) r
    | v > a = Branch a l (insert v r)

fromList l = foldr insert Empty l

preorder Empty = []
preorder (Branch a l r) = [a] ++ preorder l ++ preorder r

inorder Empty = []
inorder (Branch a l r) = inorder l ++ [a] ++ inorder r

buildFromPreIn [] [] = Empty
buildFromPreIn (h:t) il = Branch h (buildFromPreIn pleft ileft) (buildFromPreIn pright iright)
    where
    (ileft, _:iright) = splitAt (maybe 0 id $elemIndex h il) il
    (pleft, pright) = splitAt (length ileft) t
