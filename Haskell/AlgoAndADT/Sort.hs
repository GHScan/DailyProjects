import Utils
import Data.List
import Control.Monad

main = do
    let funcs = [("qsort",(128*1024),qsort),
                 ("qsort2",(128*1024),qsort2),
                 ("mergeSort",(128*1024),mergeSort),
                 ("insertionSort",(4*1024),insertionSort),
                 ("bubbleSort",(1*1024),bubbleSort)]
    testCorrectness funcs
    benchmark funcs

------------------------------
testCorrectness :: [(String,Int,([Int]->[Int]))] -> IO ()
testCorrectness funcList = do
    let testData = [randomList len (0,len) len |len<-[0,1,3,4,5,7,8,9,14,15,16,20,31,32,33,64,127,256,257]]
    let orderedTestData = map sort testData
    forM funcList (\(name,maxlen,f) -> do
        let processedData = map f testData
        let correct = and $ zipWith (==) processedData orderedTestData
        if correct then return ()
        else
            print ((error $"invalid:" ++ name) ::Int)
        )
    print "pass correctness testing ..."
------------------------------
benchmark :: [(String,Int,([Int]->[Int]))] -> IO ()
benchmark funcList = do
    let testData = [("ordered 1K", [1..1024]),
                    ("half-repeat 1K", (replicate 512 0) ++ (randomList 512 (0,512) 0)),
                    ("full-repeat 1K", (replicate 1024 0)),
                    ("random 1K", (randomList 1024 (0,1024) 1)),
                    ("random 4K", (randomList (4*1024) (0,4*1024) 1)),
                    ("random 32K", (randomList (32*1024) (0,32*1024) 1)),
                    ("random 128K", (randomList (128*1024) (0,128*1024) 1))]
    forM testData (\(dataName, d) -> do
            let funcs = filter (\(fname,maxlen,f) -> maxlen >= (length d)) funcList
            print $ dataName ++ ":"
            forM funcs (\(fname, maxlen, f) -> do
                    putStr $ "   " ++ fname ++ " : "
                    timeIt $ f d
                )
        )
    print "pass benchmark..."

------------------------------
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [v|v<-xs,v<=x] ++ [x] ++ qsort [v|v<-xs,v>x]

qsort2 :: (Ord a) => [a] -> [a]
qsort2 l@[] = l
qsort2 l@[x] = l
qsort2 (x:xs) = qsort2 [v|v<-xs2,v<x2] ++ x2:[v|v<-xs2,v==x2] ++ qsort2 [v|v<-xs2,v>x2]
    where (left,(x2:right)) = splitAt ((length xs) `div` 2) xs
          xs2 = left ++ x:right

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] right = right
merge left [] = left
merge left@(lx:lxs) right@(rx:rxs) 
    | lx <= rx = lx:merge lxs right
    | otherwise = rx: merge left rxs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort l@[] = l
mergeSort l@[x] = l
mergeSort l = merge (mergeSort left) (mergeSort right)
    where (left,right) = splitAt ((length l) `div` 2) l

insertionSort :: (Ord a) => [a] -> [a]
insertionSort l = foldl insert [] l
    where insert [] v = [v]
          insert l@(x:xs) v
            | v <= x = v:l
            | otherwise = x:insert xs v

bubble :: (Ord a) => [a] -> [a]
bubble l@[] = l
bubble l@[x] = l
bubble (x:xs) 
    | x <= x2 = x:x2:xs2
    | otherwise = x2:x:xs2
    where (x2:xs2) = bubble xs

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort l = x:bubbleSort xs
    where (x:xs) = bubble l
