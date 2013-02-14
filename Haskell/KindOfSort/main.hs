import Data.List

main = putStrLn $ show output
output = selectionSort "2398471203478923"


selectionSort [] = []
selectionSort l = m:selectionSort right
    where 
        m = minimum l
        right = delete m l


_down l
    | length l <= 1 = l
_down (h:t) = a:b:t2
    where
       (h2:t2) = _down t
       (a,b) = if h < h2 then (h,h2) else (h2,h)
bubbleSort [] = []
bubbleSort l = h:bubbleSort t
    where
     (h:t) = _down l


l1 `_merge` [] = l1
[] `_merge` l2 = l2
l1@(h1:t1) `_merge` l2@(h2:t2) 
    | h1 <= h2 = h1:(t1 `_merge` l2)
    | h1 > h2 = h2:(l1 `_merge` t2)
_merge l1 l2 = l1
mergeSort l
    | length l <= 1 = l
mergeSort l = mergeSort left `_merge` mergeSort right
    where
    (left,right) = splitAt (length l `div` 2) l
