-- | The convex hull computation algorithm using Graham scan.
module Math.Algorithms.ConvexHull2 ( convexHull2 )
where

import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Monoid

import Math.Types.Orientation
import Math.Types.PointVector2

-- | Convex hull in two dimensions using the Graham scan.
convexHull2 :: (Ord a, RealFloat a) => [Point2 a] -> [Point2 a]
convexHull2 [] = []
convexHull2 [p] = [p]
convexHull2 [p, q] = [p, q]
convexHull2 ps = scan [p0] sortedPs
    where p0 = lowestY ps
          sortedPs = sortBy (comparing $ argP p0) ps
          scan [] _ = []
          scan xs [] = xs
          scan (p1:qs) (p2:p3:rs) = case orientation2 p1 p2 p3 of
              RightTurn -> scan qs (p1:p3:rs)
              Collinear -> scan (p1:qs) (p3:rs)
              _ -> scan (p2:p1:qs) (p3:rs)
          scan xs [z] = z:xs

-- | Finds the point with the lowest y-cooordinate.
lowestY :: (Ord a) => [Point2 a] -> Point2 a
lowestY = head . sortBy (comparing y `mappend` comparing x)

