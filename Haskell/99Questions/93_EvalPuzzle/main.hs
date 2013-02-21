import Data.List

main = print output
output = genEqual [2,3,5,7,11]

data ExpNode = Const Float | BinOp Char ExpNode ExpNode deriving (Show)

ops = "+-*/"
getOpPrior '+' = 1
getOpPrior '-' = 1
getOpPrior '*' = 5
getOpPrior '/' = 5

reduceNodesList nodesL
    | all ((==1).length) nodesL = nodesL
reduceNodesList nodesL = reduceNodesList$ concatMap reduceNodes nodesL
    where
    reduceNodes nodes@[n] = [nodes]
    reduceNodes nodes = concatMap buildAt [0..length nodes - 2]
        where 
        buildAt i = [left++BinOp op h1 h2:right|op<-ops]
            where
            (left,h1:h2:right) = splitAt i nodes
        
buildTreeList l = map head $ reduceNodesList [map Const l] 

getTreeVal (Const i) = i
getTreeVal (BinOp op l r)
    | op == '+' = lv + rv
    | op == '-' = lv - rv
    | op == '*' = lv * rv
    | op == '/' && rv == 0 = lv / 0.000001
    | op == '/' = lv / rv
    where (lv,rv) = (getTreeVal l, getTreeVal r)

getTreeStr pPrior (Const i) = show $ floor i
getTreeStr pPrior (BinOp op l r) = if curProir<pPrior then '(':raws++")" else raws
    where
    curProir = getOpPrior op
    raws = getTreeStr curProir l ++ [op] ++ getTreeStr (curProir+1) r

genEqual l = concatMap genAt [1..length l - 1]
    where
    genAt i = [getTreeStr 0 lt++'=':getTreeStr 0 rt|(lt,lv)<-toValTreeList left,(rt,rv)<-toValTreeList right,lv==rv]
        where
        (left,right) = splitAt i l
        toValTreeList l = map (\t->(t,getTreeVal t)) $ buildTreeList l
