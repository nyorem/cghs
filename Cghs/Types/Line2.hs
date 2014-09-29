-- | Lines in two dimensions.
module Cghs.Types.Line2
where

import Cghs.Types.PointVector2

-- | A line is represented by a point and a vector.
type Line2 a = (Point2 a, Vector2 a)

-- | A line has the following equation: a*x + b*y + c = 0
type LineEquation2 a = (a, a, a)

-- | Direction of a line.
data LineDirection2 = HorizontalLine
                    | VerticalLine
                    | IncreasingLine
                    | DecreasingLine
                    deriving Show

-- | Computes the equation of a line.
computeEq2 :: (Num a) => Line2 a -> LineEquation2 a
computeEq2 (o, dir) = (a, b, c)
    where a = yv dir
          b = - (xv dir)
          c = -a * (x o) - b * (y o)

-- | Tests if two lines are parallel.
isParallel2 :: (Eq a, Num a) => Line2 a -> Line2 a -> Bool
isParallel2 (_, dir) (_, dir') = collinear dir dir'

-- | Computes the line which is perpendicular to the current one.
perpendicularLine2 :: (Num a) => Line2 a -> Line2 a
perpendicularLine2 (o, dir) = (o, perpendicularDir)
    where perpendicularDir = Vector2 $ (- (yv dir), xv dir)

-- | Predicate that determines if a point lies on a line.
onTheLine2 :: (Eq a, Num a) => Point2 a -> Line2 a -> Bool
onTheLine2 p l = (a * (x p) + b * (y p) + c) == 0
    where (a, b, c) = computeEq2 l

-- | Maybe returns the intersection point of two lines.
intersectLines2 :: (Eq a, Fractional a) => Line2 a -> Line2 a -> Maybe (Point2 a)
intersectLines2 l1 l2
    | isParallel2 l1 l2 = Nothing
    | otherwise = Just $ Point2 ( (c2 * b1 - c1 * b2) / det, (a2 * c1 - a1 * c2) / det )
    where (a1, b1, c1) = computeEq2 l1
          (a2, b2, c2) = computeEq2 l2
          det = a1 * b2 - a2 * b1

-- | Computes the direction of a line.
directionLine2 :: (Eq a, Num a) => Line2 a -> LineDirection2
directionLine2 (_, dir)
    | collinear dir (Vector2 (1, 0)) = HorizontalLine
    | collinear dir (Vector2 (0, 1)) = VerticalLine
    | signum (xv dir) == signum (yv dir) = IncreasingLine
    | otherwise = DecreasingLine

-- | A line parallel to the X axis.
xaxis :: (Num a) => Point2 a -> Line2 a
xaxis o = (o, Vector2 (1, 0))

-- | A line parallel to the Y axis.
yaxis :: (Num a) => Point2 a -> Line2 a
yaxis o = (o, Vector2 (0, 1))

