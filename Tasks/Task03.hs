import Data.List
import Numeric.Natural

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

--3.3

data Component a = Component a Natural
type Storage a = [Component a]
data Product a b = Product a [(b, Natural)]

instance Eq a => Eq (Component a) where
    (Component a1 n1) == (Component a2 n2) = a1 == a2 

contains :: Eq a => Storage a -> a -> Maybe Natural
contains [] comp = Nothing
contains ((Component a n) : tail) searchVal
    | a == searchVal = Just n
    | otherwise = contains tail searchval

store :: Eq a => Storage a -> a -> Natural -> Storage a
store [] searchVal number =  [(Component searchVal number)]
store ((Component a n): tail) searchVal number
    | a == searchVal = (Component a (n+number)) : tail -- gefunden
    | otherwise = (Component a n) : store tail searchVal number


remove :: Eq a => Storage a -> a -> Natural -> Storage a1
remove [] _ _ = []
remove ((Component a n ): tail) searchval number
    | a == searchVal && n > number = (Component a (n-number)) : tail
    | a == searchVal && n <= number = tail
    | otherwise = (Component a n) : store tail searchVal number



isProducible :: (Eq a, Eq b) => Product a b -> Storage b -> Bool
isProducible (Product a []) _ = True 
isProducible _ [] = False
isProducible (Product name ((ingred_name, ingred_amount):tail)) storage 
    | contains ingred_name storage == Nothing = False 
    | contains ingred_name storage >= Just ingred_amount = isProducible (Product name tail) storage
    | otherwise = False

produce :: (Eq a, Eq b) => Product a b -> Storage b -> Storage b
produce (Product a ((name, num):t)) storage = produce (Product a t) (remove storage name num)
produce (Product a []) storage = storage
produce _ [] = []


--3.4

{-
ghci> "" == []
True da ein string eine char Liste ist und "" eine leere Liste

-> String ist 

ghci> tail [1] == ""
<interactive>:2:7: error: [GHC-39999]
    * No instance for `Num Char' arising from the literal `1'
    * In the expression: 1
      In the first argument of `tail', namely `[1]'
      In the first argument of `(==)', namely `tail [1]'


-> Fehler da [1] eine Int liste ist und tail eine leere int liste zurückgibt. == braucht jedoch zwei listen gleichen typs

ghci> tail [1] == []
<interactive>:3:1: warning: [GHC-63394] [-Wx-partial]
    In the use of `tail'
    (imported from Prelude, but defined in GHC.Internal.List):
    "This is a partial function, it throws an error on empty lists. Replace it with 'drop' 1, or use pattern matching or 'GHC.Internal.Data.List.uncons' instead. Consider refactoring to use "Data.List.NonEmpty"."

True

true da [] polymorph ist und für alle leeren Listen im Vergleich genutzt werden kann



ghci> read "10" == 10
True -> 10 wird zu Int konvertiert
ghci> read "10"
*** Exception: Prelude.read: no parse
Fehler -> Compiler weiß nicht in welchen Typ 10 umgewandelt werden soll da z.b. der Vergleich wie im Beispiel fehlt
-}

--3.5 
data Set a = Set [a]

createSet :: Eq a => [a] -> Set a
createSet [] = Set []
createSet list = Set (createSet' list [])
    where 
        createSet' (h:t) curr 
            | elem h curr = createSet' t curr
            | otherwise = createSet' t (h:curr)
        createSet' [] curr = curr


union :: Eq a => Set a -> Set a -> Set a
union (Set a) (Set b) = createSet (a ++ b)


intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set a ) (Set b) = createSet(findIntersect (Set a) (Set b))
    where 
        findIntersect (Set (h:t)) (Set s) 
            | elem h b = h : findIntersect (Set t) (Set s)
            | otherwise = findIntersect(Set t) (Set s)
        findIntersect _ _ = []


-- Set a \ b 
setMinus :: Eq a => Set a -> Set a -> Set a
setMinus (Set []) _ = Set []
setMinus (Set (h:t)) (Set b)
    | elem h b = setMinus (Set t) (Set b)
    | otherwise = union (Set h) (setMinus (Set t) (Set b))
