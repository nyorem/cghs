-- | Polygons in two dimensions.
module Math.Types.Polygon2
where

import Math.Types.PointVector2

-- | We consider a polygon to be a list of points: its vertices.
type Polygon2 a = [Point2 a]

