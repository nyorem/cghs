-- | Circles in two dimensions.
module Cghs.Types.Circle2
where

import Cghs.Types.PointVector2
import Cghs.Types.Segment2
import Cghs.Utils

-- | A circle is made with an origin and a radius.
type Circle2 a = (Point2 a, Double)

-- | Is a point inside a circle?
isInCircle2 :: (Floating a, Ord a) => Point2 a -> Circle2 a -> Bool
isInCircle2 p (o, r) = squaredNorm (o .-. p) <= realToFrac (r * r)

-- | Intersection between a segment and a circle.
intersectSegmentCircle2 :: (Floating a, Ord a) => Segment2 a -> Circle2 a -> Maybe [Point2 a]
intersectSegmentCircle2 s (o, r) =
    case maybeSols of
        Nothing -> Nothing
        Just sols -> let points = map (constructPoint (src s) d) sols in
                         if all (\p -> insideSegment2 p s) points then Just points else Nothing
    where d = (dst s) .-. (src s)
          e = (src s) .-. o
          a = squaredNorm d
          b = 2 * (d <.> e)
          c = (squaredNorm e) - realToFrac (r * r)
          maybeSols = solveQuadratic a b c
          constructPoint start dir t = start .+> (t *.> dir)

-- | Does a segment intersects a circle in two points?
doesSegmentIntersectCircle :: (Floating a, Ord a) => Segment2 a -> Circle2 a -> Bool
doesSegmentIntersectCircle s c =
    case sols of
        Nothing -> False
        Just xs -> length xs == 2
        where sols = intersectSegmentCircle2 s c

