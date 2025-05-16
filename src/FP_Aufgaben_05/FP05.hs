module FP05 (

)
where

type Noten = [(Int, (Int, Int))]

-- Aufgabe 1
noten :: Noten
noten =
    [ (1, (1, 2))
    , (2, (2, 3))
    , (3, (3, 4))
    , (4, (4, 5))
    , (5, (5, 6))
    ]

gibMatrikelNummer :: (t -> Bool) -> [(a1, (a2, t))] -> [a1]
gibMatrikelNummer _ [] = []
gibMatrikelNummer f ((m, (_, y)) : xs)
    | f y = m : gibMatrikelNummer f xs
    | otherwise = gibMatrikelNummer f xs

x = gibMatrikelNummer (< 3) noten

-- Aufgabe 2
erhoeheNote :: (Int -> Int) -> Noten -> Noten
erhoeheNote _ [] = []
erhoeheNote addiere1 ((m, (x, y)) : xs)
    | x >= 10 = (m, (x, addiere1 y)) : erhoeheNote addiere1 xs
    | otherwise = erhoeheNote addiere1 xs
