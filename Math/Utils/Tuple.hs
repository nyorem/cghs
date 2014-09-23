-- | Utility functions on tuples.
module Math.Utils.Tuple
where

-- | Applies a function to a couple.
(><) :: (a -> b) -> (a, a) -> (b, b)
f >< (x, y) = (f x, f y)

