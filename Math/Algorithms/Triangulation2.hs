-- | Triangulations in two dimensions.
module Math.Algorithms.Triangulation2
where

import Math.Types.PointVector2
import Math.Types.Polygon2
import Math.Types.Triangle2

-- | Triangulation of a polygon.
triangulatePolygon2 :: (Num a) => Polygon2 a -> [Triangle2 a]
triangulatePolygon2 p = undefined

-- | Triangulation of a point set.
triangulatePointSet2 :: (Num a) => [Point2 a] -> [Triangle2 a]
triangulatePointSet2 ps = undefined
