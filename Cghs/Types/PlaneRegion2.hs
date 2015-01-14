-- | Regions of the plane.
module Cghs.Types.PlaneRegion2
where

import Data.Monoid

import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.Ray2
import Cghs.Types.Segment2

-- | A region of the plane is determined by a boundary
-- and two rays (for the extremities).
data PlaneRegion2 a = PlaneRegion2 { leftDir :: Ray2 a
                                   , boundary :: [Point2 a]
                                   , rightDir :: Ray2 a
                                   } deriving Show

-- | Computes the edges of a plane region.
edgesPlaneRegion2 :: (Num a) => PlaneRegion2 a -> [Line2 a]
edgesPlaneRegion2 (PlaneRegion2 l b r) = [rayToLine2 l, rayToLine2 r] ++ zipWith (\ p q -> segmentToLine2 $ Segment2 p q) (init b) (tail b)

-- | Determines if a point is inside a region or not.
isInsidePlaneRegion2 :: (Num a, Ord a) => Point2 a -> PlaneRegion2 a -> Bool
isInsidePlaneRegion2 p reg = mconcat sides == head sides
    where sides = map (sideLine2 p) $ edgesPlaneRegion2 reg

-- | Computes the intersection of a plane region with a line.
intersectPlaneRegionLine2 :: (Eq a, Fractional a) => PlaneRegion2 a -> Line2 a -> Maybe [Point2 a]
intersectPlaneRegionLine2 reg l
    | null inter = Nothing
    | otherwise = sequence inter
    where inter = map (intersectLines2 l) $ edgesPlaneRegion2 reg

