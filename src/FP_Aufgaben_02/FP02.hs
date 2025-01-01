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
bonbon :: Int -> Int
bonbon = bonbon' 10
  where
    bonbon' preis geld
      | geld < preis = 0
      | geld >= preis = 1 + bonbon' (preis + 10) (geld - (preis `mod` 100))
      -- geld >= preis = 1 + bonbon' (if preis == 100 then 10 else preis + 10) (geld - preis)
      | otherwise = -1
