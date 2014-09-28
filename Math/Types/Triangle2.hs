-- | Triangles in two dimensions.
module Math.Types.Triangle2
where

import Math.Types.Line2
import Math.Types.PointVector2

-- | The 2D triangle data type.
data Triangle2 a = Triangle2 {p1 :: Point2 a,
                              p2 :: Point2 a,
                              p3 :: Point2 a} deriving (Show, Eq)

-- | Circumcenter of a triangle.
circumcenter :: (Eq a, Fractional a, Num a) => Triangle2 a -> Maybe (Point2 a)
circumcenter t = intersectLines face face'
    where face  = (p1 t, (p2 t) .-. (p1 t))
          face' = (p2 t, (p3 t) .-. (p3 t))

