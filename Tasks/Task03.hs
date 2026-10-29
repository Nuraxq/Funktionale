import Data.List


--3.1
splitL :: Int -> [a] -> [[a]]
splitL n xs = [take (n+1) xs, drop (n+1) xs]


mSplitL :: [Int] -> [a] -> [[a]]
mSplitL [] xs 
    | null xs = [] 
    | otherwise = [xs]
mSplitL (number:rest) list = take (number+1) list : mSplitL' (drop (number+1) list) rest (number+1)
    where 
        mSplitL' liste (number:rest) current = take (number+1-current) liste : mSplitL' (drop (number+1-current) liste) rest (number+1)
        mSplitL' liste _ _ = [liste]


--3.2 
sortier :: Ord a => [a] -> [a]
sortier [] = []
sortier list = minimum list : sortier (del (minimum list) list)


del :: Eq a => a -> [a] -> [a]
del el (h:t)
    | el == h = t
    | otherwise = h: (del el t)
    


data ListInt = ListInt [Int] Int deriving(Show)

create :: [Int] -> ListInt
create [] = ListInt [] 0
create h = ListInt h (sum h)


getList :: ListInt -> [Int]
getList (ListInt h _) = h

getSum :: ListInt -> Int
getSum (ListInt _ n) = n

add :: Int -> ListInt -> ListInt
add n (ListInt h m) =  ListInt (n:h) (n+m)

liconcat :: ListInt -> ListInt -> ListInt
liconcat (ListInt l1 n1) (ListInt l2 n2) = (ListInt (l1++l2) (n1+n2))


-- listint eq 
instance Eq ListInt where
    (ListInt list num) == (ListInt list2 num2) = (sortier(list) == sortier(list2)) && (num == num2)

instance Ord ListInt where 
    (ListInt l1 n1) <= (ListInt l2 n2)
        | length l1 < length l2 = True
        | length l1 > length l2 = False
        | otherwise = n1 <= n2