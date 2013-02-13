import qualified Map

main = putStrLn $ show output
output = Map.toList $ Map.fromList [(3,9),(1,1),(5,25),(2,4),(4,16)]
