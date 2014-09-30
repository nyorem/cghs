-- | Polygons in two dimensions.
module Cghs.Types.Polygon2
where

import Cghs.Types.PointVector2

-- | We consider a polygon to be a list of points: its vertices
-- given in counter-clockwise order.
type Polygon2 a = [Point2 a]

-- | Gets the sides of a polygon.
polygonSides2 :: Polygon2 a -> [(Point2 a, Point2 a)]
polygonSides2 [] = []
polygonSides2 poly@(p1:ps) = zip poly (ps ++ [p1])

-- | Determines if a point is inside a polygon.
isInsidePolygon2 :: (Fractional a, Ord a) => Polygon2 a -> Point2 a -> Bool
isInsidePolygon2 [] _ = False
isInsidePolygon2 poly point = foldr step False couples
    where couples = zip poly (last poly : init poly)
          step (p, q) c =
              if (y p > y point) /= (y q > y point) &&
                 (x point < x p + ((x q - x p) * (y point - y p)) / (y q - y p) )
              then not c
              else c

