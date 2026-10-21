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


data Format = Flushleft | Flushright
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


takeWord :: String -> String
takeWord s 
    | s == "" = ""
    | head s == ' ' = ""
    | otherwise = head s : takeWord (tail s) 


fillWords :: String -> Words
fillWords s     
    | s == "" = EmptyWords
    | head s == ' ' = fillWords (tail s)
    | otherwise = NotEmptyWords (takeWord s) (fillWords (drop (length (takeWord s)) s))


getWordsUntil :: Words -> Int -> String -> Int -> String
getWordsUntil EmptyWords _ _ _= ""
getWordsUntil (NotEmptyWords s rest) n curr maxLen
    | null curr = s ++ getWordsUntil rest (n-(length s)) s maxLen
    | (length s) < n = " " ++ s ++ getWordsUntil rest (n-(length s)-1) s maxLen
    | otherwise = "\n" ++ getWordsUntil (NotEmptyWords s rest) maxLen "" maxLen

align :: Format -> Int -> String -> String

align Flushleft n s = getWordsUntil (fillWords s) n "" n  

align Flushright _ _ = ""


