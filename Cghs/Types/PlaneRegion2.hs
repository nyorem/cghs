-- | Regions of the plane.
module Cghs.Types.PlaneRegion2
where

import Cghs.Types.PointVector2
import Cghs.Types.Ray2

data PlaneRegion2 a = PlaneRegion2 { leftDir :: Ray2 a,
                                     boundary :: [Point2 a],
                                     rightDir :: Ray2 a } deriving Show

-- | Determines if a point is inside a region or not.
isInsidePlaneRegion2 :: Point2 a -> PlaneRegion2 a -> Bool
isInsidePlaneRegion2 p r = undefined

