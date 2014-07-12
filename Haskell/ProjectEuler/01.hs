main = print output

output = sum [v|v<-[3..999],v `mod` 3 == 0 || v `mod` 5 == 0]
