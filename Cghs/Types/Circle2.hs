-- | Circles in two dimensions.
module Cghs.Types.Circle2
where

import Data.Maybe ( isJust )

import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.Segment2
import Cghs.Utils

-- | A circle is made with an origin and a radius.
type Circle2 a = (Point2 a, Double)

-- | Is a point inside a circle?
isInCircle2 :: (Floating a, Ord a) => Point2 a -> Circle2 a -> Bool
isInCircle2 p (o, r) = squaredNorm (o .-. p) <= realToFrac (r * r)

-- | Intersection between a line and a circle.
intersectLineCircle2 :: (Floating a, Ord a) => Line2 a -> Circle2 a -> Maybe [Point2 a]
intersectLineCircle2 (s, d) (o, r) =
    case maybeSols of
        Nothing -> Nothing
        Just sols -> Just $ map (constructPoint s d) sols
    where e = s .-. o
          a = squaredNorm d
          b = 2 * (d <.> e)
          c = (squaredNorm e) - realToFrac (r * r)
          maybeSols = solveQuadratic a b c
          constructPoint start dir t = start .+> (t *.> dir)

-- | Does a segment intersects a circle in two points?
doesSegmentIntersectCircle2 :: (Floating a, Ord a) => Segment2 a -> Circle2 a -> Bool
doesSegmentIntersectCircle2 s c =
    case sols of
        Nothing -> False
        Just points -> all (\p -> insideSegment2 p s) points
        where sols = intersectLineCircle2 (segmentToLine2 s) c

-- | Does a line intersects a circle in two points?
doesLineIntersectCircle2 :: (Floating a, Ord a) => Line2 a -> Circle2 a -> Bool
doesLineIntersectCircle2 l c = isJust (intersectLineCircle2 l c)

