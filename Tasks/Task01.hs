--1.1
summe :: Integer -> Integer
summe n = if n <= 0 then 0 else n + summe(n-1)

--1.2
digit :: Integer -> Integer
digit n 
    | n < 0 = (-1)*digit((-n))
    | n == 0 = 0
    | otherwise = (mod n 10) + digit(div n 10)

--1.3
isPrefixOf :: String->String->Bool
isPrefixOf s t 
    | null s = True
    | null t = False
    | (head s == head t) = isPrefixOf (tail s)  (tail t)
    | otherwise = False

--1.4
digit2 :: Integer -> Integer
digit2 n =
    if n < 0 then (-1)*digit2(-n)
    else if n == 0 then 0
    else (mod n 10) + digit2(div n 10)

isPrefixOf2 :: String->String->Bool
isPrefixOf2 s t =
    if null s then True
    else if null t then False
    else if (head s == head t) then (isPrefixOf2 (tail s ) (tail t))
    else False

-- Methode 1: Statt otherwise einfach nur else 
-- Methode 2: Einfach True nehmen als Bedingung 

{- 1.5
strikte Funktionen: Sobald ein Argument undef ist, ist das Ergebnis undef. Alle Argumente werden ausgewertet.
nicht Strikte Funktionen: Argumente werden nur ausgewertet wenn sie benötigt werden. 
Obwohl ein Argument undef wäre, kann die Funktion trotzdem Terminieren (muss aber nicht)



strikte Sprachen: Sprachen die Standartmäßig die strikte Funktionsauswertung machen
nicht strikte Sprachen: Sprachen die eine verzögerte Auswertung haben, berechnen Ausdrücke und Argumente erst sobald es nötig ist.



Nicht striktheit vs Laziness: Nicht strikt ist die Eigenschaft einer Funktion, dass sie trotz undef Argumenten terminieren kann.
Laziness ist eine Auswertungsstrategie, wie Argumente ausgewertet werden. 
Kurz gefasst - Nicht striktheit ist eine Eigenschaft und Laziness eine Strategie. 


Beispiele nicht strikt:
Nicht striktes Konstrukt: || bzw. &&, sobald der Ausdruck eindeutig true oder false ist wird der Rest nicht ausgewertet. 
Ja es ist sehr Sinnvoll um Rechenzeit zu sparen und Fehlerbehandlung leichter zu machen, gibt es z.b. eine Divison in einer Abfrage kann man vorher immer if x != 0  schreiben.  
-}

