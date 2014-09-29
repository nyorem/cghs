-- | Triangles in two dimensions.
module Cghs.Types.Triangle2
where

import Cghs.Types.Line2
import Cghs.Types.PointVector2

-- | The 2D triangle data type.
--
-- The triangle is defined by: p1 -> p2 -> p3 -> p1
data Triangle2 a = Triangle2 {p1 :: Point2 a,
                              p2 :: Point2 a,
                              p3 :: Point2 a} deriving (Show, Eq)

-- | Circumcenter of a triangle.
circumcenter :: (Eq a, Fractional a) => Triangle2 a -> Maybe (Point2 a)
circumcenter t = intersectLines2 face face'
    where face  = (p1 t, (p2 t) .-. (p1 t))
          face' = (p2 t, (p3 t) .-. (p3 t))

-- | Determines if a point is inside a triangle.
isInsideTriangle :: (Num a) => Triangle2 a -> Point2 a -> Bool
isInsideTriangle t p = undefined

