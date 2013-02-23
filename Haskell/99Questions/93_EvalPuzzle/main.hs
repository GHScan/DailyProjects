import Data.List

main = print output
output = puzzle [2,3,5,7,11]

data ExpNode = Const Float | BinExp Char ExpNode ExpNode deriving (Show)

ops = "+-*/"
getOpPrior '+' = 1
getOpPrior '-' = 1
getOpPrior '*' = 5
getOpPrior '/' = 5

evalByOp '+' l r = l + r
evalByOp '-' l r = l - r
evalByOp '*' l r = l * r
evalByOp '/' l r 
    | r == 0 = l / 0.00001
    | otherwise = l / r

getNodeStr pPrior (Const f) = show $ floor f
getNodeStr pPrior (BinExp op l r) = if curPrior<pPrior then '(':str++")" else str
    where
    curPrior = getOpPrior op
    str = getNodeStr curPrior l ++ op: getNodeStr (curPrior + 1) r

splits l = init$ tail$ zip (inits l) (tails l)

toExpList [x] = [(x, Const x)]
toExpList l = [(evalByOp op lv rv,BinExp op lt rt)|(left, right)<-splits l,(lv,lt)<-toExpList left,(rv,rt)<-toExpList right,op<-ops]

puzzle l = [getNodeStr 0 lt ++ '=':getNodeStr 0 rt|(left,right)<-splits l,(lv,lt)<-toExpList left,(rv,rt)<-toExpList right,lv==rv]
