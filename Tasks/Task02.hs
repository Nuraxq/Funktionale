-- Aufgabe 2.1

data Jeton = RedJeton | GreenJeton | BlueJeton | SilverJeton | GoldJeton
    
value RedJeton = 1
value GreenJeton = 5
value BlueJeton = 10
value SilverJeton = 50
value GoldJeton = 100


data Jetons = EmptyJetons | JetonsConstructor Jeton Jetons 

count :: Jetons -> Int
count EmptyJetons = 0
count (JetonsConstructor j rest) = 1 + count rest

payoff :: Jetons -> Int 
payoff EmptyJetons = 0
payoff (JetonsConstructor j rest) = value j + payoff rest


buy :: Int -> Jetons
buy amount 
    | amount >= 100 = JetonsConstructor GoldJeton (buy (amount-100)) 
    | amount >= 50  = JetonsConstructor SilverJeton (buy (amount-50))
    | amount >= 10 = JetonsConstructor BlueJeton (buy (amount-10))
    | amount >= 5 = JetonsConstructor GreenJeton (buy (amount-5))
    | amount >= 1 = JetonsConstructor RedJeton (buy (amount-1))
    | otherwise = EmptyJetons    


{-
-- Dachte ich brauche das aber anscheinend nicht
takeUntil :: String -> Char -> Int -> String
takeUntil s c n
    | null s = "" 
    | n <= 0 && c == head s = ""  
    | otherwise = head s : takeUntil (tail s) c (n-1)

-- Dachte hier auch ich brauche das aber doch nicht so
dropUntil :: String -> Char -> Int -> String
dropUntil s c n
    | null s = "" 
    | n <= 0 && c == head s = tail s
    | otherwise = dropUntil (tail s) c (n-1)





-- Neuer versuch 2.2:
-- Nimmt ein Wort raus (leerzeichenfrei)


-- Nimmt so viele Wörter (mit Leerzeichentrennung) bis keins mehr Rein passt. Wenn keins mehr passt wird mit Leerzeichen aufgefüllt. 
takeWordsUntil :: String -> Int-> String -> String
takeWordsUntil s n curr 
    | s == "" = ""
    | head s == ' ' = ' ': takeWordsUntil (tail s) (n-1)
    | length (takeWord s) <= n = takeWord s : takeWordsUntil (drop length(takeWord s) s) (n- length(takeWord s))
    | length (takeWord s)> n = replicate n ' '

-}


-- Versuch 3 Wortliste erstmal anlegen

data Words = EmptyWords | NotEmptyWords String Words 

data Format = Flushleft | Flushright

takeWord :: String -> String
takeWord s 
    | s == "" = ""

    | head s == ' ' = ""
    | otherwise = head s : takeWord (tail s) 

-- Trennt einen Satz in Wörter 
fillWords :: String -> Words
fillWords s     
    | s == "" = EmptyWords
    | head s == ' ' = fillWords (tail s)
    | otherwise = NotEmptyWords (takeWord s) (fillWords (drop (length (takeWord s)) s))

-- Macht basically ganz Flush left. Kriegt eine Wort Liste geben und erzeugt daraus die Linksbündige Formattierung
flHelper :: Words -> Int -> String -> Int -> String
flHelper EmptyWords _ _ _= ""
flHelper (NotEmptyWords s rest) n curr maxLen
    | null curr = s ++ flHelper rest (n-(length s)) s maxLen -- Wort ist alleine in der Zeile
    | (length s) < n = " " ++ s ++ flHelper rest (n-(length s)-1) s maxLen -- Wort passt noch rein
    | otherwise = "\n" ++ flHelper (NotEmptyWords s rest) maxLen "" maxLen -- Wort passt nicht rein 

-- Wird mit lines aufgerufen
-- Füllt einzeln jede Zeile auf und konkateniert am Ende alles
frHelper :: [String] -> Int ->  String
frHelper [] _ = ""
frHelper [h] n = fillLeft h n 
frHelper (h:t) n = (fillLeft h n) ++ "\n" ++ frHelper t n 

-- Füllt auf Länge n auf
fillLeft :: String -> Int -> String
fillLeft s n 
    | length s >= n = s  
    | otherwise = (replicate (n-(length s)) ' ') ++ s


align :: Format -> Int -> String -> String

align Flushleft n s = flHelper (fillWords s) n "" n  

align Flushright n s = frHelper (lines (flHelper (fillWords s) n "" n)) n  


--2.3

safeDiv :: Maybe Double -> Maybe Double -> Maybe Double
safeDiv Nothing Nothing = Nothing
safeDiv Nothing _ = Nothing
safeDiv _ Nothing = Nothing
safeDiv _ (Just 0) = Nothing
safeDiv (Just n) (Just m) = Just(n/m)

safeRoot :: Maybe Double -> Maybe Double
safeRoot Nothing = Nothing
safeRoot (Just n) 
    | n < 0 = Nothing
    | n >= 0 = sqrt n

-- Warum ist es Sinnvoll? Man muss bei der Division keine Gültigkeit checken, es wird immer ein gültiges Ergebnis geliefert

--2.5

data ListInt = ListInt [Int] Int deriving(Show)

create :: [Int] -> ListInt
create [] = [] 0
create [h] = ListInt h (sum h)


getList :: ListInt -> [Int]
getList (ListInt h n) = h

getSum :: ListInt -> Int
getSum (ListInt h n) = n

add :: Int -> ListInt -> ListInt
add n (ListInt h m) =  ListInt (n:h) (n+m)

liconcat :: ListInt -> ListInt -> ListInt





--2.4:
-- Das 0-Tuple gibt es auch noch (), er hat immer den gleichen Wert. Er ist Sinnvoll wenn man nichts zurückgeben kann wie bei einer IO operation. In Haskell muss jedoch jede Funktion etwas zurückgeben. 