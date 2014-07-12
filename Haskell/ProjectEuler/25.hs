main = print $ output 1000

fibs a b = a:fibs b (a + b)

output n = fst $ head $ dropWhile ((< n).length.snd) $ zip [1..] $ map show $ fibs 1 1
