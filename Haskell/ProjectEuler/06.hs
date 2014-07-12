
main = print $ output 100

output n = sqr (sum [1..n]) - sum [v*v|v<-[1..n]]
    where sqr x = x * x
