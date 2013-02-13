
module List (
    List(Empty),
    pushBack,
    pushFront,
    fromList,
    toList,
) where

data List v = Node v (List v) | Empty deriving (Show)

pushBack :: v->List v->List v
pushBack v Empty = Node v Empty
pushBack v (Node val next) = Node val $ pushBack v next

pushFront :: v->List v->List v
pushFront v Empty = Node v Empty
pushFront v n = Node v n

fromList :: [v] -> List v
fromList l = foldr pushFront Empty l

toList :: List v-> [v]
toList Empty = []
toList (Node v next) = v : toList next
