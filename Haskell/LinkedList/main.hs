import qualified List

main = putStrLn $ show output
output = List.toList $ List.pushBack 11 $ List.fromList [1..10]
