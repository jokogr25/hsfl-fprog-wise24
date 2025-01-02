module FP02
  (
  )
where

-- Aufgabe 1
-- a)
-- f1 nimmt einen Parameter x vom Typ Double und gibt einen Wert vom Typ Double zurück
f1 x = 3.14 + x

-- b)
-- die Funktion f2 nimmt einen Parameter x vom Typ Char und gibt einen Wert vom Typ Int zurück
-- es wird hier ein sog. Guard (|) verwendet: dadurch kann man im geprüften pattern noch einmal Bedinungen prüfen
f2 x
  | x == 'A' = 0

-- Aufgabe 2
-- linear rekursiv bedeutet, dass nur ein Rekursionsaufruf erfolgt
-- http://www.tilman.de/uni/ws03/alp/rekursion.php
potenz :: Int -> Int -> Int
potenz _ 0 = 1
potenz x e = x * potenz x (e - 1)

-- Aufgabe 3
-- es wurde hier modulo verwendet, da es dann möglich ist, den Preis in jedem Schritt weiter zu erhöhen (220 mod 100 = 20)
bonbon :: Int -> Int
bonbon = bonbon' 10
  where
    bonbon' preis geld
      | geld < preis = 0
      | geld >= preis = 1 + bonbon' (preis + 10) (geld - (preis `mod` 100))
      -- geld >= preis = 1 + bonbon' (if preis == 100 then 10 else preis + 10) (geld - preis)
      | otherwise = -1

-- Aufgabe 4
-- in der Aufgabe wird ein Hinweis gegeben, dass die Funktion mittels summe und laenge implementiert werden soll - diese müssen selbst implementiert werden
mittelwert :: Int -> Int -> Double
mittelwert _ 0 = 0
mittelwert a b = fromIntegral (summe a b) / fromIntegral (laenge a b)

summe :: Int -> Int -> Int
summe a b
  | a > b = 0
  | otherwise = a + summe (a + 1) b

laenge :: Int -> Int -> Int
laenge a b
  | a > b = 0
  | otherwise = 1 + laenge (a + 1) b
