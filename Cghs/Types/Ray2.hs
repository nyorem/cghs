-- | Rays in two dimensions.
module Cghs.Types.Ray2
where

import Cghs.Types.Line2
import Cghs.Types.PointVector2

-- | A ray is composed of an origin (point) and a direction (vector).
data Ray2 a = Ray2 { originRay :: Point2 a,
                     dirRay :: Vector2 a } deriving Show

-- | Converts a ray to a line.
rayToLine2 :: Ray2 a -> Line2 a
rayToLine2 (Ray2 o d) = (o, d)

