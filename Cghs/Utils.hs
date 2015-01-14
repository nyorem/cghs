-- | Utility functions used in the library.
module Cghs.Utils
where

import Data.List ( delete )

-- | First component of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Gets the successor in a cyclic way.
succ' :: (Bounded a, Enum a, Eq a) => a -> a
succ' s
    | s == maxBound = minBound
    | otherwise = succ s

-- | Gets the predecessor in a cyclic way.
pred' :: (Bounded a, Enum a, Eq a) => a -> a
pred' s
    | s == minBound = maxBound
    | otherwise = pred s

-- | Get the i'th element of a list by considering the list to be cyclic.
(<!!>) :: [a] -> Int -> a
xs <!!> i = xs !! i'
    where i' = i `mod` (length xs)

-- | Deletes multiple elements in a list.
deleteList :: (Eq a) => [a] -> [a] -> [a]
deleteList xs [] = xs
deleteList xs (d:ds) = deleteList (delete d xs) ds

-- | Solves the quadratic equation given by: a * x^2 + b * x + c = 0.
solveQuadratic :: (Floating a, Ord a) => a -> a -> a -> Maybe [a]
solveQuadratic a b c
    | delta == 0 = Just [- b / (2 * a)]
    | delta > 0 = Just [ (-b - sqrt (delta)) / (2 * a), (-b + sqrt (delta)) / (2 * a)]
    | otherwise = Nothing
    where delta = b * b - 4 * a * c

