import Data.Time.Clock
import Control.DeepSeq
import Data.Monoid
import Control.Monad

------------------------------
timeIt :: (NFData a) => a -> IO ()
timeIt e = do
    start <- getCurrentTime
    end <- e `deepseq` getCurrentTime
    print $ diffUTCTime end start

------------------------------

main = do
    let testData = replicate 2000 [1..100]
    let funcs = [("concat", concat),
                 ("leftConcat", leftConcat),
                 ("rightConcat", rightConcat),
                 ("leftConcatWithDlist", leftConcatWithDlist),
                 ("rightConcatWithDlist", rightConcatWithDlist)]
    forM_ funcs (\(fname, f) -> do
        putStr $fname ++ " : "
        timeIt $ length $f testData
        )

------------------------------
newtype Dlist a = Dlist (a->a)

toDlist :: (Monoid a) => a -> Dlist a
toDlist l = Dlist (l `mappend`)

fromDlist :: (Monoid a)  => Dlist a -> a
fromDlist (Dlist f) = f mempty

dlistConcat :: Dlist a -> Dlist a -> Dlist a
dlistConcat (Dlist f1) (Dlist f2) = Dlist (f1 . f2)

------------------------------
leftConcat :: (Monoid a) => [a] -> a
leftConcat l = foldl mappend mempty l

rightConcat :: (Monoid a) => [a] -> a
rightConcat l = foldr mappend mempty l

leftConcatWithDlist :: (Monoid a) => [a] -> a
leftConcatWithDlist l = fromDlist $ foldl dlistConcat (toDlist mempty) $ map toDlist l

rightConcatWithDlist :: (Monoid a) => [a] -> a
rightConcatWithDlist l = fromDlist $ foldr dlistConcat (toDlist mempty) $ map toDlist l
