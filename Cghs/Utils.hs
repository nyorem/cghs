-- | Utility functions used in the library.
module Cghs.Utils
where

import Control.Monad ( unless )

-- | Same as 'maybe' but the order of parameters is changed.
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = maybe nothingRes f m

-- | 'unless' with a monadic predicate.
unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

-- | Applies a function to a couple.
(><) :: (a -> b) -> (a, a) -> (b, b)
f >< (x, y) = (f x, f y)

-- | First component of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | 'if then else' with a function.
bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

