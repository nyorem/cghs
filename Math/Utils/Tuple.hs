-- | Utility functions on tuples.
module Math.Utils.Tuple
where

-- | Applies a function to a couple.
(><) :: (a -> b) -> (a, a) -> (b, b)
f >< (x, y) = (f x, f y)

-- | First component of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

