{-# LANGUAGE Rank2Types #-}

-- | Circles in two dimensions.
module Math.Types.Circle2
where

import Math.Types.PointVector2

-- | A circle is made with an origin and a radius.
type Circle2 a = (Floating r) => (Point2 a, r)

-- | Is a point inside a circle?
isInCircle :: (Floating a, Ord a) => Circle2 a -> Point2 a -> Bool
isInCircle (o, r) p = squaredNorm op <= square r
    where square n = n * n
          op = o .-. p

