module FP03
  (
  )
where

-- Aufgabe 1
f1 :: (Double, Char) -> [(Double, Char)]
f1 x = x : [(1.3, 'a')]

-- Aufgabe 2
-- diese Funktion ist fehlerhaft, da sich der Rückgabetyp der Pattern unterscheidet:
-- im ersten Pattern wird geprüft, ob der erste Parameter eine 0 ist und dann wird eine leere Liste zurückgegeben
-- im zweiten Pattern wird der zweite Parameter auf 1 geprüft und dann der erste Parameter (x) zuückgegeben - dieser wurde im ersten Pattern aber bereits als Int (bzw. Num) definiert und kann nicht als Rückgabetyp Liste dienen

-- Aufgabe 3
addListen :: [Int] -> [Int] -> [Int]
addListen [] ys = ys
addListen xs [] = xs
addListen (x : xs) (y : ys) = (x + y) : addListen xs ys

-- Aufgabe 4
umrechnen :: Int -> [(Int, Int)]
umrechnen betrag =
  let muenzen :: [Int]
      muenzen = [200, 100, 50, 20, 10, 5, 2, 1]

      passtMuenzeInBetrag :: Int -> Int -> (Int, Int)
      passtMuenzeInBetrag x muenze = (x `div` muenze, muenze)
   in snd
        ( foldl
            ( \(b, erg) muenze ->
                ( b - (b `div` muenze) * muenze,
                  erg ++ [passtMuenzeInBetrag b muenze]
                )
            )
            (betrag, [])
            muenzen
        )

ausgabe :: [(Int, Int)] -> [(Int, Int)]
ausgabe [] = []
ausgabe ((anzahl, muenze) : xs) =
  if anzahl > 0
    then (anzahl, muenze) : ausgabe xs
    else ausgabe xs

-- | betrag >= 200 =(betrag `div` 200,200) : umrechnen (betrag `mod` 200)
-- | betrag >= 100 =(betrag `div` 100,100) : umrechnen (betrag `mod` 100)
-- | betrag >= 50 =(betrag `div` 50,50) : umrechnen (betrag `mod` 50)
-- | betrag >= 20 =(betrag `div` 20,20) : umrechnen (betrag `mod` 20)
-- | betrag >= 10 =(betrag `div` 10,10) : umrechnen (betrag `mod` 10)
-- | betrag >= 5 =(betrag `div` 5,5) : umrechnen (betrag `mod` 5)
-- | betrag >= 2 =(betrag `div` 2,2) : umrechnen (betrag `mod` 2)
-- | betrag >= 1 =(betrag `div` 1,1) : umrechnen (betrag `mod` 1)

-- Aufgabe 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

fibListe :: Int -> [Int]
fibListe 0 = []
fibListe x = fibListe (x - 1) ++ [fib x]
