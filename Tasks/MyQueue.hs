module MyQueue(
    MyQueue,
    push,
    pop,
    peek,
    empty   
) where 

    
import qualified Deque as D


newtype MyQueue a = MyQueue (D.Deque a)

empty :: MyQueue a
empty = MyQueue (D.makeDequeFromList [])

push :: a -> MyQueue a -> MyQueue a
push a (MyQueue  x) = MyQueue (D.pushEnd a x)

pop :: MyQueue a -> MyQueue a
pop (MyQueue x) = MyQueue (D.popFront x)

peek :: MyQueue a -> Maybe a
peek (MyQueue x) = D.peekFront x

-- Laufzeiten O(1) für alles außer Push 