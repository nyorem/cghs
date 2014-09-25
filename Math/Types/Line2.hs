-- | Lines in two dimensions.
module Math.Types.Line2
where

import Math.Types.PointVector2

-- | A line is represented by a point and a vector.
type Line2 a = (Point2 a, Vector2 a)

-- | A line has the following equation: a*x + b*y + c = 0
type LineEquation2 a = (a, a, a)

-- | Computes the equation of a line.
computeEq :: (Num a) => Line2 a -> LineEquation2 a
computeEq (o, dir) = (a, b, c)
    where a = yv dir
          b = - (xv dir)
          c = -a * (x o) - b * (y o)

-- | Predicate that determines if a point lies on a line.
onTheLine :: (Eq a, Num a) => Point2 a -> Line2 a -> Bool
onTheLine p l = (a * (x p) + b * (y p) + c) == 0
    where (a, b, c) = computeEq l

-- | Maybe returns the intersection point of two lines.
intersectLines :: (Eq a, Fractional a, Num a) => Line2 a -> Line2 a -> Maybe (Point2 a)
intersectLines l1 l2
    | det == 0 = Nothing -- parallel lines
    | otherwise = Just $ Point2 (c2 * b1 - c1 * b2 / det, a2 * c1 - a1 * c2 / det)
    where (a1, b1, c1) = computeEq l1
          (a2, b2, c2) = computeEq l2
          det = a1 * b2 - a2 * b1

