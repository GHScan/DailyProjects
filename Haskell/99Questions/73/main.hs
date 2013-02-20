import Data.List
main = putStrLn $ show output
output = t == (fromString$toString t)
    where
    t = Node 'a' [ Node 'f' [Node 'g' []], Node 'c' [], Node 'b' [Node 'd' [], Node 'e' []] ]

data Tree a = Node a [Tree a] deriving (Show,Eq)

toString (Node a []) = [a]
toString (Node a l) = ['(',a] ++ concatMap ((' ':).toString) l ++ [')']

fromString s = head$fst$_fromString s
    where
    _fromString (' ':t) = _fromString t
    _fromString l@(')':t) = ([],l)
    _fromString ('(':h:t) = ([Node h $concatMap fst raws],rest)
        where
        raws = takeWhile (not.null.fst) $tail $iterate (\(l,rest)->_fromString rest) ([],t)
        rest = tail$snd$last raws
    _fromString (h:t) = ([Node h []],t)
