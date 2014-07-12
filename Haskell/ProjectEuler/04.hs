main = print output

output = maximum [x*y|x<-[100..999],y<-[x..999],let z=show $x * y,z==reverse z]
