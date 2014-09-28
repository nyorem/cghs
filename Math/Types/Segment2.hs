-- | Segments in two dimensions.
module Math.Types.Segment2
where

import Math.Types.Line2
import Math.Types.PointVector2

-- | The 2D segment data type.
data Segment2 a = Segment2 {src :: Point2 a,
                            dst :: Point2 a} deriving (Show, Eq)

-- | Length of a segment.
lengthSegment :: (Floating a) => Segment2 a -> a
lengthSegment s = sqrt . squaredNorm $ p .-. q
    where p = src s
          q = dst s

-- | Predicate that determines if a point is inside a segment.
insideSegment :: (Ord a, Num a) => Point2 a -> Segment2 a -> Bool
insideSegment p s = (onTheLine p (segmentToLine s)) &&
              ((x p) >= (x . src $ s) && (x p) <= (x . dst $ s)) &&
              ((y p) >= (y . src $ s) && (y p) <= (y . dst $ s))

-- | Middle of a segment.
middle :: (Fractional a) => Segment2 a -> Point2 a
middle s = Point2 $ ((x p + x q) / 2, (y p + y q) / 2)
    where p = src s
          q = dst s

-- | Converts a segment to a line.
segmentToLine :: (Num a) => Segment2 a -> Line2 a
segmentToLine s = (src s, dst s .-. src s)

-- | Computes the perpendicular bisector of a segment.
bisector :: (Fractional a) => Segment2 a -> Line2 a
bisector s = (middle s, dir)
    where (_, dir) = perpendicularLine $ segmentToLine s


