--4.1

product' :: [Int] -> Int

product' liste = foldr (*) 1 liste


allOdd :: [Int] -> Bool
allOdd liste = (length liste) == (length (filter odd liste))


xSquaredPlusThreeXPlusFive :: [(Integer,Integer)]
xSquaredPlusThreeXPlusFive = map (\x -> (x, x^2 + 3*x+ 5)) [1..150]


getByKey :: [(String,Int)] -> String -> [Int]

getByKey liste a = concatMap (\(k,v) -> if k == a then [v] else []) liste 



--4.2
{-
Beispiel: Python
_ ist ein legaler Bezeichner, wird aber meist als Wegwerfvariable genutzt

In einem match Konstrukt erfüllt _ jedoch die gleiche Funktion wie in Haskell, es akzeptiert alle Werte und ist ein Platzhalter


let f _ _ funktioniert, da der Unterstrich ein Wildcard Pattern ist, jedes _ ist unabhängig von den anderen 
let g x x funnktioniert nicht da Variablennamen auf der linken Seite einer Funktion nur einmal vorkommen dürfen. 

b) 

foldl  :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl1 :: Foldable t => (a -> a -> a)       -> t a -> a

foldr  :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr1 :: Foldable t => (a -> a -> a)       -> t a -> a


foldl und foldr nehmen ein Funktionsargument als Startwert entgegen, bei foldr1 und fold1 ist es jeweils das erste Element in der Liste 
Da ein Listenelement der Akkumulator, der Startwert ist muss der Ergebnistyp der Funktion damit übereinstinmen
Das heißt wenn man z,B. Strings aus einer Int Liste erstellen will geht das nicht, denn das Ergebnis ist ein String, erwartet wird jedoch Int (Typ des ersten Elements)


-}



--4.3
nat :: [Integer]
nat = [0..]

ev :: [Integer]
ev = filter even nat 

harmonic :: [Double]
harmonic = map (\x -> if x == 0 then 0 else  1 / fromIntegral x) nat

triangle :: [Integer]
triangle = map (\x -> if x == 0 then 0 else sum [1..x]) nat

palin :: [Integer]
palin = filter isPalin [0..]


isPalin :: Integer -> Bool
isPalin n = s == reverse s 
    where s = show n

--4.4

dfold :: (b -> c -> c) -> c -> (a -> b -> b) -> b -> [[a]] -> c


dfold f i g j x = foldr f i (map (foldr g j) x)


nfilter :: [(a -> Bool)] -> [a] -> [a]


nfilter ps x = foldr filter x ps 


-- 4.5

f_a :: [Integer -> Integer]
f_a = map (\a -> \x  -> x*x+x+a ) nat

f_all = map (\x -> map x nat) f_a


ttake :: Int -> Int -> [[a]] -> [a]

ttake a b f_all = take a (f_all !! b)