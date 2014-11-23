-- | Triangles in two dimensions.
module Cghs.Types.Triangle2
where

import Cghs.Types.Circle2
import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.Segment2

import Data.Maybe ( fromJust )

-- | The 2D triangle data type.
--
-- The triangle is defined by the sequence of vertices: p1 -> p2 -> p3 -> p1
data Triangle2 a = Triangle2 {p1 :: Point2 a,
                              p2 :: Point2 a,
                              p3 :: Point2 a} deriving (Show, Eq)

-- | Signed area of a triangle.
signedAreaTriangle2 :: (Fractional a) => Triangle2 a -> a
signedAreaTriangle2 t = ((x r - x q) * y p + (x p - x r) * y q + (x q - x p) * y r) / 2
    where p = p1 t
          q = p2 t
          r = p3 t

-- | Circumcenter of a triangle: intersection of the bisectors.
circumcenter :: (Eq a, Fractional a) => Triangle2 a -> Point2 a
circumcenter t = fromJust $ intersectLines2 bis1 bis2
    where bis1  = bisector2 $ Segment2 (p1 t) (p2 t)
          bis2  = bisector2 $ Segment2 (p1 t) (p3 t)

-- | Circumcircle of a triangle.
circumcircle :: (Eq a, Fractional a, Floating a, Real a) => Triangle2 a -> Circle2 a
circumcircle t = (center, realToFrac radius)
    where center = circumcenter t
          radius = distancePointLine2 center (segmentToLine2 $ Segment2 (p1 t) (p2 t))

-- | Determines if a point is inside a triangle.
isInsideTriangle2 :: (Fractional a, Ord a) => Triangle2 a -> Point2 a -> Bool
isInsideTriangle2 triangle point = s > 0 && t > 0 && s + t < 2 * a * sign
    where a = signedAreaTriangle2 triangle
          sign = signum a
          p = p1 triangle
          q = p2 triangle
          r = p3 triangle
          s = (y p * x r - x p * y r + (y r - y p) * x point + (x p - x r) * y point) * sign
          t = (x p * y q - y p * x q + (y p - y q) * x point + (x q - x p) * y point) * sign

-- | Determines if a point is inside the circumcircle
-- of a triangle.
isInsideCircumcircle2 :: (Eq a, Fractional a, Floating a, Real a) => Triangle2 a -> Point2 a -> Bool
isInsideCircumcircle2 t p = isInCircle2 p (circumcircle t)

