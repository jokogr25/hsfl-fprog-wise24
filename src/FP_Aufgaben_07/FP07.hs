module FP03
  (
  )
where

-- Aufgabe 1
-- Was ist das Ergebnis von f 5?
-- f = \ x -> (4 * x + 1) `div` 2
-- Was steht hier eigentlich?
-- Aufruf: f 5
-- --> (\ x -> (4 * x + 1) `div` 2) 5
-- --> (\ 5 -> (4 * 5 + 1) `div` 2)
-- --> (4 * 5 + 1) `div` 2
-- --> 21 `div` 2
-- --> 10

-- Aufgabe 2
-- Welchen Typ haben die Funktionen?
-- f1 :: [Int] -> [Int] (denn: (++) :: [a] -> [a] -> [a])
-- f1 = (++ [1, 2, 3])

-- Aufgabe 3
-- Der Akkumulator ist der Wert, der das Ergebnis "akkumuliert" ()
-- Wie dreht man eine Liste um? Man nimmt den aktuellen Wert (xn) und klebt
-- diesen an den Beginn der Akkumulator-Liste.im nächsten Durchlauf wird (xn+1)
-- dann an den Beginn des Akkumulators gesetzt und so wird die Liste Stück für
-- Stück umgedreht
-- Um eine Fehlbenutztung eine*r Anwender*in zu verhindern, wird eine lokale Funktion in einem where-Block definiert, in der die Akkumulator-Technik angewandt wird. Die Unterfunktion wird dann mit einer leeren Liste "initialisiert"
-- das Pattern "reverseIntList = reverseIntList' []" ist eta-reduziert, eigentlich steht dort "reverseIntList xs = reverseIntList' [] xs"
reverseIntList :: [Int] -> [Int]
reverseIntList = reverseIntList' []
  where
    reverseIntList' :: [Int] -> [Int] -> [Int]
    reverseIntList' akk [] = akk
    reverseIntList' akk (x : xs) = reverseIntList' (x : akk) xs

-- was pasiert beim Aufruf von reverseIntList [1,2,3]?
-- Aufruf: reverseIntList [1,2,3]
-- --> reverseIntList' [] [1,2,3]
-- --> reverseIntList' (1 : []]) [2,3]
-- --> reverseIntList' [1] [2,3]
-- --> reverseIntList' (2 : [1]) [3]
-- --> reverseIntList' [2,1] [3]
-- --> reverseIntList' (3 : [2,1]) []
-- --> reverseIntList' [3,2,1] []
-- --> [3,2,1]

-- Aufgabe 4
-- da eine Summe berechnet werden soll, ist der Startwert des Akkumulators 0
f :: Int -> Int
f n = foldr (\b a -> a + (b * b)) 0 [1 .. n]

-- Aufgabe 5
liste :: [(Integer, [(String, Double)])]
liste =
  [ ( 1,
      [ ("FPROG", 2.0),
        ("ELM", 3.0),
        ("OOP", 1.7)
      ]
    ),
    ( 2,
      [ ("FPROG", 5.0),
        ("ELM", 4.0),
        ("OOP", 2.3)
      ]
    )
  ]

gibNotenkonto :: Integer -> [(Integer, [(String, Double)])] -> [(String, Double)]
gibNotenkonto _ [] = error "Matrikelnummer nicht vorhanden"
gibNotenkonto matrikelNummer ((mNr, noten) : xs)
  | matrikelNummer == mNr = noten
  | otherwise = gibNotenkonto matrikelNummer xs

bestanden :: [(String, Double)] -> Int
bestanden [] = 0
bestanden ((_, note) : xs) = (if note <= 4.0 then 1 else 0) + bestanden xs
