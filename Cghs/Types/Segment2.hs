-- | Segments in two dimensions.
module Cghs.Types.Segment2
where

import Data.Maybe ( fromJust, isNothing )

import Cghs.Types.Line2
import Cghs.Types.PointVector2

-- | The 2D segment data type.
data Segment2 a = Segment2 {src :: Point2 a,
                            dst :: Point2 a} deriving (Show, Eq)

-- | Length of a segment.
lengthSegment2 :: (Floating a) => Segment2 a -> a
lengthSegment2 s = sqrt . squaredNorm $ (src s) .-. (dst s)

-- | Predicate that determines if a point is inside a segment.
insideSegment2 :: (Num a, Ord a) => Point2 a -> Segment2 a -> Bool
insideSegment2 p s = (onTheLine2 p (segmentToLine2 s)) &&
              (((x p) >= (x . src $ s) && (x p) <= (x . dst $ s)) &&
              ((y p) >= (y . src $ s) && (y p) <= (y . dst $ s))) ||
              (((x p) >= (x . dst $ s) && (x p) <= (x . src $ s)) &&
              ((y p) >= (y . dst $ s) && (y p) <= (y . src $ s)))

-- | Middlepoint of a segment.
middlepoint2 :: (Fractional a) => Segment2 a -> Point2 a
middlepoint2 s = Point2 $ ((x p + x q) / 2, (y p + y q) / 2)
    where p = src s
          q = dst s

-- | Converts a segment to a line.
segmentToLine2 :: (Num a) => Segment2 a -> Line2 a
segmentToLine2 s = (src s, dst s .-. src s)

-- | Computes the perpendicular bisector of a segment.
bisector2 :: (Fractional a) => Segment2 a -> Line2 a
bisector2 s = (middlepoint2 s, dir)
    where (_, dir) = perpendicularLine2 $ segmentToLine2 s

-- | Converts a line to a segment.
lineToSegment2 :: (Eq a, Fractional a) => Line2 a -> Segment2 a
lineToSegment2 l@(o, _) = Segment2 p q
    where bottom = intersectLines2 l $ xaxis $ Point2 (0 , -1)
          up     = intersectLines2 l $ xaxis $ Point2 (0 ,  1)
          left   = intersectLines2 l $ yaxis $ Point2 (-1,  0)
          right  = intersectLines2 l $ yaxis $ Point2 (1 ,  0)
          (p, q) = case directionLine2 l of
            HorizontalLine -> ( Point2 (-1, y o), Point2 (1, y o) )
            VerticalLine   -> ( Point2 (x o, -1), Point2 (x o, 1) )
            IncreasingLine -> if isNothing up then (fromJust right, fromJust bottom) else (fromJust left, fromJust up)
            DecreasingLine -> if isNothing up then (fromJust left, fromJust bottom) else (fromJust right, fromJust up)

