main = print $ output 100

output n = sum $ map (read .(:[])) $ show $ product [1..n]
