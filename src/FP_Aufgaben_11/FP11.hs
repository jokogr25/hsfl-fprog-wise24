{-# LANGUAGE FlexibleInstances #-}

module FP11
  (
  )
where

data Ausdruck a
  = Zahl a
  | Add (Ausdruck a) (Ausdruck a)
  | Sub (Ausdruck a) (Ausdruck a)

class Rechner a where
  rechne :: a -> Double

instance Rechner (Ausdruck Double) where
  rechne (Zahl x) = x
  rechne (Add a b) = rechne a + rechne b
  rechne (Sub a b) = rechne a - rechne b

instance Eq (Ausdruck Double) where
  a == b = rechne a == rechne b

-- g =
--   let f = \x y -> (/) x y
--    in f 2 (3 + 1)

-- g' :: (Fractional a) => a
-- g' = f 2 (3 + 1)
--   where
--     f :: (Fractional a) => a -> a -> a
--     f = \x y -> (/) x y

-- f =
--   let g = filter (\x -> x < 5)
--    in g [1, 5, 9, 4, 2]

-- f' :: (Ord a, Num a) => [a]
-- f' = g [1, 2, 2, 3, 3, 34]
--   where
--     g :: (Ord a, Num a) => [a] -> [a]
--     g = filter (\x -> x < 5)
