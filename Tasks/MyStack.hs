module MyStack(
    MyStack,
    empty,
    push,
    pop,
    peek
)where

import qualified Deque as D


newtype MyStack a = MyStack (D.Deque a)

empty :: MyStack a
empty = MyStack (D.makeDequeFromList [])

push :: a -> MyStack a -> MyStack a
push a (MyStack x) = MyStack (D.pushFront a x)

pop :: MyStack a -> MyStack a
pop (MyStack x) = MyStack (D.popFront x)

peek :: MyStack a -> Maybe a
peek (MyStack  x) = D.peekFront x

-- O(1) für alles 