import Data.List

main = putStrLn $ show output
output = head $knightVisit 7 (1,1)

knightVisit n start = visit [start]
    where
    grids = [(x,y)|x<-[1..n],y<-[1..n]]
    jumps = [(p1,p2)|p1@(x1,y1)<-grids,p2@(x2,y2)<-grids,x1/=x2,abs (x1-x2)+abs (y1-y2)==3]
    visit path
        | length path == n * n = [reverse path]
    visit path = concatMap (\p2->visit $p2:path) validNextPos
        where
        validNextPos = [p2|(p1,p2)<-jumps,p1 == head path,not$elem p2 path]
