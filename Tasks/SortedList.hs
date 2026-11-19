module SortedList(
    SortedList,
    empty,
    insert,
    head,
    tail,
    length,
    null
) where

import Prelude hiding (null, length,tail,head)
import qualified Prelude as P


newtype SortedList a = SortedList [a]

empty :: SortedList a 
empty = SortedList []

insert::Ord a => a -> SortedList a -> SortedList a
insert element (SortedList xs) = SortedList (insertList element xs)

insertList :: Ord a => a -> [a] -> [a]
insertList element [] = [element]
insertList element (x:xs)
    | element > x = x : (insertList element xs)
    | otherwise = element : (x:xs)

null :: SortedList a -> Bool
null (SortedList []) = True
null (SortedList xs) = False

length :: SortedList a -> Int
length (SortedList a) = P.length a


head :: SortedList a -> Maybe a
head (SortedList (x:xs)) = Just x
head (SortedList []) = Nothing

tail :: SortedList a -> Maybe (SortedList a)
tail (SortedList (x:xs)) = Just (SortedList xs)
tail (SortedList []) = Nothing

instance Show a => Show (SortedList a) where
  show (SortedList xs) = show xs