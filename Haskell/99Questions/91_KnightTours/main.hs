import Data.List

main = putStrLn $ show output
output = head $knightVisit 5 (1,1)

isKnightJump dx dy = adx > 0 && adx + ady == 3
    where 
    (adx,ady) = (abs dx,abs dy)

knightVisit n start = visit [start]
    where
    grids = [(x,y)|x<-[1..n],y<-[1..n]]
    jumps = [(p1,p2)|p1@(x1,y1)<-grids,p2@(x2,y2)<-grids,isKnightJump (x1-x2) (y1-y2)]
    visit path
        | length path == n * n = [path]
    visit path = concatMap (\jump->visit$path ++ [snd jump]) validJumps
        where
        validJumps = [jump|jump@(p1,p2)<-jumps,p1 == last path,all (/=p2) path]
