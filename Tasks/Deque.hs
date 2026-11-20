
module Deque(
    Deque,
    pushFront,
    pushEnd,
    peekFront,
    peekEnd,
    popFront,
    popEnd,
    makeDequeFromList
) where

newtype Deque a = Deque [a]

peekFront :: Deque a -> Maybe a
peekFront (Deque (x:xs)) = Just x
peekFront(Deque []) = Nothing


peekEnd :: Deque a -> Maybe a
peekEnd (Deque []) = Nothing 
peekEnd (Deque x) = Just (last x)

popFront :: Deque a -> Deque a
popFront (Deque (x:xs)) = Deque xs
popFront (Deque _) = Deque []
 
makeDequeFromList :: [a] -> Deque a
makeDequeFromList xs = Deque xs

pushFront :: a -> Deque a -> Deque a
pushFront a (Deque x) = Deque (a:x)

pushEnd :: a -> Deque a -> Deque a
pushEnd a (Deque x) = Deque (x ++ [a])

popEnd :: Deque a -> Deque a
popEnd (Deque []) = Deque []
popEnd (Deque x) = Deque (init x)

