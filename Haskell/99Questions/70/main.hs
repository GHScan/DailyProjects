import Data.List
main = putStrLn $ show output
output = s == (tree2string $string2tree s)
    where 
    s = "afg^^c^bd^e^^^"

data Tree a = Node a [Tree a] deriving (Show)

pushChild (Node a l) n = Node a (l++[n])
getChildren (Node a l) = l

tree2string (Node a l) = [a] ++ concatMap tree2string l ++ "^"

string2tree s = head$getChildren $head$_string2tree [Node '_'[]] s 
    where
    _string2tree stack [] = stack
    _string2tree stack ('^':t) = _string2tree (stack3 ++ [pushChild _last2 _last]) t
        where
        (stack2, _last) = (init stack, last stack)
        (stack3, _last2) = (init stack2, last stack2)
    _string2tree stack (h:t) = _string2tree (stack++[Node h[]]) t
