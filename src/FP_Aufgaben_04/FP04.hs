module FP04
  (
  )
where

-- Aufgabe 1
-- f 0 x y = y ++ [x]
-- f x y z = x : y : z
f :: Int -> Int -> [Int] -> [Int]
f 0 x y = y ++ [x]
f x y z = x : y : z

-- Aufgabe 3
ersetzeDurchX :: [Char] -> [Char]
ersetzeDurchX [] = []
ersetzeDurchX (x : xs) = (if x == 'e' then 'X' else x) : ersetzeDurchX xs

ersetzeDurchX' :: [Char] -> [Char]
ersetzeDurchX' = ersetzeDurchX'' []
  where
    ersetzeDurchX'' :: [Char] -> [Char] -> [Char]
    ersetzeDurchX'' akk [] = akk
    ersetzeDurchX'' akk (x : xs) =
      ersetzeDurchX''
        (akk ++ [if x == 'e' then 'X' else x])
        xs
