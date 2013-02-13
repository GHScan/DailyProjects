module Map (
    Map(Empty),
    insert,
    lookup,
    delete,
    fromList,
    toList,
) where

import Prelude hiding(lookup)

data Map k v = Node k v (Map k v) (Map k v) | Empty deriving (Show)

insert :: (Ord k, Eq k) => k -> v-> Map k v -> Map k v
insert k v Empty = Node k v Empty Empty
insert k v node@(Node nk nv l r) 
    | k == nk = node
    | k < nk = Node nk nv (insert k v l) r
    | k > nk = Node nk nv l (insert k v r)

lookup :: (Ord k, Eq k) => k -> Map k v -> Maybe v
lookup k Empty = Nothing
lookup k (Node nk nv l r)
    | k == nk = Just nv
    | k < nk = lookup k l
    | k > nk = lookup k r

delete :: (Ord k, Eq k) => k -> Map k v -> Map k v
delete k Empty = Empty
delete k (Node nk nv l r)
    | k == nk = fromList $ toList l ++ toList r
    | k < nk = Node nk nv (delete k l) r
    | k > nk = Node nk nv l (delete k r)

fromList :: (Ord k, Eq k) => [(k,v)] -> Map k v
fromList l = foldr (\(k,v) m->insert k v m) Empty l

toList :: (Ord k, Eq k) => Map k v -> [(k,v)]
toList Empty = []
toList (Node k v l r) = toList l ++ [(k,v)] ++ toList r
