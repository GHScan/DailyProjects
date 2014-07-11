import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import Data.List
import Data.Function
import System.Process

interning1 = do
    contents <- getContents
    let r = foldl' (\m (line,c)->Map.insertWith (+) line c m) Map.empty . map (\line->(line,1)) . lines $ contents
    return r

interning2 = do
    contents <- ByteString.getContents
    let lines = filter (ByteString.notElem 0x0a) $ ByteString.groupBy ((==) `on` (==0x0a)) contents
    let r = foldl' (\m (line,c)-> Map.insertWith (+) line c m) Map.empty . map (\line->(line,1)) $ lines
    return r

main = do 
    m <- interning2
    print $ Map.size m
    r <- system "ps -v"
    return (Map.size m)
