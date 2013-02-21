import Data.List

main = putStrLn $ show output
output = head $queens 8

queens n = _queens n n
    where
    _queens n 1 = map (:[]) [1..n]
    _queens n i = concatMap branch $_queens n (i - 1)
        where
        branch sub = [ x:sub |x<-[1..n], not.elem x $sub, all (\(i,y)->abs (i-1) /= abs (x-y)) $ zip [2..] sub]
