main = print output

perm [] = [[]]
perm l = [x:rest|i<-[0..length l - 1],let (left,(x:right))=splitAt i l, rest<-perm (left++right)]

output = perm [0..9] !! 999999
