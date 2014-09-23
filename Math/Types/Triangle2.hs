-- | Triangles in two dimensions.
module Math.Types.Triangle2
where

import Math.Types.PointVector2

-- | The 2D triangle data type.
data Triangle2 a = Triangle2 {p1 :: Point2 a,
                              p2 :: Point2 a,
                              p3 :: Point2 a} deriving (Show, Eq)

