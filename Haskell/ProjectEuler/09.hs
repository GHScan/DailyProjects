
main = print $ product $ last $ filter ((== 1000) . sum) $ output 500

output n = [[a,b,c]|a<-[1..n],b<-[a..n],c<-[b..n],a*a+b*b==c*c]
