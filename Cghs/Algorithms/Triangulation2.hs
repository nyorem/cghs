-- | Triangulations in two dimensions.
module Cghs.Algorithms.Triangulation2
where

import Cghs.Types.PointVector2
import Cghs.Types.Polygon2
import Cghs.Types.Triangle2

-- | Triangulation of a polygon.
triangulatePolygon2 :: (Num a) => Polygon2 a -> [Triangle2 a]
triangulatePolygon2 p = undefined

-- | Triangulation of a point set.
triangulatePointSet2 :: (Num a) => [Point2 a] -> [Triangle2 a]
triangulatePointSet2 ps = undefined
