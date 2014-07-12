import Utils

main = print $ output 500

output n = fst $ head $dropWhile ((< n) . snd) $ map (\n->(n,Utils.factorCount n)) $ scanl1 (+) [1..]
