
main = putStrLn $ show output
output = getPosition $Branch 'n' (Branch 'k' (Branch 'c' (Branch 'a' Empty Empty) (Branch 'e' (Branch 'd' Empty Empty) (Branch 'g' Empty Empty))) (Branch 'm' Empty Empty)) (Branch 'u' (Branch 'p' Empty (Branch 'q' Empty Empty)) Empty)

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show)

getHeight Empty = 0
getHeight (Branch _ l r) = 1 + max (getHeight l) (getHeight r)
getPosition t = map (\(a,(x,y))->(a,(x+off,y))) rawList
    where
    _getPosition Empty x y n = []
    _getPosition t@(Branch a l r) x y n = _getPosition l (x - 2^n) (y + 1) (n - 1) ++ [(a,(x,y))] ++ _getPosition r (x + 2^n) (y + 1) (n - 1)
    rawList = _getPosition t 0 1 (getHeight t - 2)
    off = (1-) $fst$snd$head $rawList
