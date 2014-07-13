import Control.Applicative
import Control.Monad
import Data.Monoid

import qualified Data.Map as Map

------------------------------
newtype State s a = State {getState::s->(s,a)}

instance Functor (State s) where
    fmap nf (State f) = State $(\s-> nf <$> f s)

instance Applicative (State s) where
    pure v = State$ \s -> (s, v)
    (State ff) <*> (State f) = State $(\s-> let (s2, nf) = ff s
                                            in nf <$> f s2)

instance Monad (State s) where
    return = pure
    (State f) >>= ff = State$ \s -> let (s2, (State g)) = ff <$> f s
                                    in g s2
------------------------------
lookupCache k def = State$ \s -> (s, Map.findWithDefault def k s)
updateCache k v = State$ \s -> (Map.insert k v s, ())

------------------------------
fib2' n = do
    v <- lookupCache n (-1)
    if v > 0 then return v
    else if n < 2 then return n
    else do
        v <- (+) <$> fib2' (n - 1) <*> fib2' (n - 2)
        updateCache n v
        return v

fib2 n = snd $ (getState$fib2' n) Map.empty

fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

main = print $ fib2 28
