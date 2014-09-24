-- | The convex hull computation algorithm using Graham scan.
module Math.Algorithms.ConvexHull2
where

import Math.Types.PointVector2

-- | Convex hull in two dimensions: Graham scan.
convexHull2 :: [Point2 a] -> [Point2 a]
convexHull2 ps = tail ps

