module Utils
(
 timeIt,
 randomList,
 ) where

import Data.Time.Clock
import Control.DeepSeq
import System.Random

timeIt :: (NFData a) => a -> IO ()
timeIt e = do
    start <- getCurrentTime
    end <- e `deepseq` getCurrentTime
    print $ diffUTCTime end start

randomList :: (Random a) => Int -> (a,a) -> Int -> [a]
randomList n range seed = genList n range (mkStdGen seed)
    where 
        genList 0 range g = []
        genList n range g = v:genList (n - 1) range newg
            where (v,newg) = randomR range g
