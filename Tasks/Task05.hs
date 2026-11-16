import Data.Bifoldable
import Data.Bifunctor

--5.1

ev = [ q | q <- [0..], even q]
harmonic = [ 1/q | q <- [0..], (\x ->  x > 0 ) q]
palin = [ q | q <- [0..], (reverse (show q) == show q)]

partsum :: Num a => [a] -> [a]
partsum l = [foldr (+) 0 (take x l)  | x <- [1..]]

--5.2

data Bool_ = True_ | False_
-- Nein kein Functor, da ein Functor den Typkonstruktur Type -> Type braucht, hier gibt es jedoch keinen Typ Parameter. Es gibt keinen Variablen inhalt 
-- Anpassbar ist er auch nicht Sinnvoll da man ja nur zwischen zwei Werten unterscheiden will. 

data List a = List a (List a) | Empty
-- Ja ist ein Functor, hat den Kind (Type -> Type).


data Either_ a b = Left_ a | Right_ b
--Ne ist nicht direkt ein Functor da es Kind (Type -> Type -> Type) hat 
-- Wenn man sich auf einen Wert festlegt kann es ein Functor sein, z.B. das Left_ a nicht gemappt werden kann, sondern nur Right_ b
-- Currying 

data Maybe_ a = Nothing_ | Just_ a
-- Ja ist ein Functor 


data Pair a b = Pair a b
-- Kein Functor, wie bei Either muss ein Wert festgelegt werden 

data LList a = LList [a] (a,a) a
-- Functor 




--5.3


instance Functor List where
    fmap f (List a as) = List (f a) (fmap f as)
    fmap _ Empty = Empty 


instance Functor (Either_ a) where
    fmap f (Left_ x) = Left_ x
    fmap f (Right_ b) = Right_ (f b) 

instance Functor Maybe_ where
    fmap f Nothing_ = Nothing_
    fmap f (Just_ a) = Just_ (f a)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Functor LList where
    fmap f (LList a (b,c) d) = LList (map f a) (f b, f c) (f d)



--5.4 

data Seq a = End
    | Seq a (Seq a)
    | Lseq [a] (Seq a)
    | Pseq (a,a) (Seq a)
    deriving (Show)

instance Foldable Seq where
    foldr f z (Seq a rest) = f a (foldr f z rest)
    foldr f z (Lseq liste rest) = foldr f (foldr f z rest) liste 
    foldr f z (Pseq (a1,a2) rest) = f a1 (f a2 (foldr f z rest))
    foldr f z End = z  

instance Functor Seq where
    fmap _ End              = End
    fmap f (Seq a rest) = (Seq (f a) (fmap f rest))
    fmap f (Lseq liste rest) = Lseq (map f liste) (fmap f rest)
    fmap f (Pseq (a1,a2) rest) = Pseq (f a1, f a2) (fmap f rest)


--5.5 
{-
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

Nimmt eine Funktion von a nach b und eine von c nach d, um dann für einen Typkonstruktor mit Kind Type->Type->Type map bereitzustellen. 
Dabei wird der erste Typparameter mit der ersten Funktion gemappt, der zweite Typparameter mit der zweitem 

class Bifoldable p where
    bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
Bei Fold, werden für einen Typkonstruktor mit Kind Type->Type->Type zwei Funktionen genommen die beide den gleichen Typ c als Ergebnis haben.
Sie nehmen jeweils ein Akkumulator vom Typ c. 
Dann werden die Typen a und b jeweils in ein Datum vom Typ c umgewandelt und zusammen gefoldet
-}

instance Bifunctor Either_ where
    bimap f _ (Left_ a) = Left_ (f a)
    bimap _ g (Right_ b) = Right_ (g b) 

instance Bifunctor Pair where
    bimap f g (Pair a b) = Pair (f a) (g b)

instance Bifoldable Either_ where 
    bifoldr _ g s (Right_ b) = g b s
    bifoldr f _ s (Left_ a) = f a s 

instance Bifoldable Pair where
    bifoldr f g s (Pair a b) = (f a (g b s))