
main = putStrLn $ show output
output = buildTree 4

data Node a = Branch a (Node a) (Node a) | Empty deriving (Show)

buildTree 0 = [Empty]
buildTree 1 = [Branch 'x' Empty Empty]
buildTree n 
    | ln == rn = [Branch 'x' l r |l<-buildTree ln, r<-buildTree rn]
    | otherwise = [Branch 'x' l r |l<-buildTree ln, r<-buildTree rn] ++[Branch 'x' l r |l<-buildTree rn, r<-buildTree ln]
    where 
    ln = (n - 1) `div` 2
    rn = (n - 1) - ln
