-- | Monad utilities.
module Math.Utils.Monad
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

