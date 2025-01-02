module FP03
  (
  )
where

-- Aufgabe 1
f1 :: (Double, Char) -> [(Double, Char)]
f1 x = x : [(1.3, 'a')]

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

-- | betrag >= 200 =(betrag `div` 200,200) : umrechnen (betrag `mod` 200)
-- | betrag >= 100 =(betrag `div` 100,100) : umrechnen (betrag `mod` 100)
-- | betrag >= 50 =(betrag `div` 50,50) : umrechnen (betrag `mod` 50)
-- | betrag >= 20 =(betrag `div` 20,20) : umrechnen (betrag `mod` 20)
-- | betrag >= 10 =(betrag `div` 10,10) : umrechnen (betrag `mod` 10)
-- | betrag >= 5 =(betrag `div` 5,5) : umrechnen (betrag `mod` 5)
-- | betrag >= 2 =(betrag `div` 2,2) : umrechnen (betrag `mod` 2)
-- | betrag >= 1 =(betrag `div` 1,1) : umrechnen (betrag `mod` 1)
