-- | The convex hull computation algorithm using Graham scan.
module Cghs.Algorithms.ConvexHull2 ( convexHull2Graham
                                   , convexHull2SweepLine )
where

import Data.List ( find, sortBy )
import Data.Ord ( comparing )
import Data.Monoid ( mappend )

import Cghs.Types.Orientation
import Cghs.Types.PointVector2
import Cghs.Types.Polygon2

import Debug.Trace

-- | Convex hull in two dimensions using the Graham scan.
convexHull2Graham :: (Ord a, RealFloat a) => [Point2 a] -> [Point2 a]
convexHull2Graham [] = []
convexHull2Graham [p] = [p]
convexHull2Graham [p, q] = [p, q]
convexHull2Graham ps = reverseOrderPolygon2 $ init $ scan [p0] sortedPs
    where p0 = lowestY ps
          sortedPs = sortBy (comparing $ argP p0) ps
          scan [] _ = []
          scan xs [] = xs
          scan (p1:qs) (p2:p3:rs) = case orientation2 p1 p2 p3 of
              RightTurn -> scan qs (p1:p3:rs)
              Collinear -> scan (p1:qs) (p3:rs)
              _ -> scan (p2:p1:qs) (p3:rs)
          scan xs [z] = z:xs

-- | Convex hull in two dimensions using a sweep line algorithm.
convexHull2SweepLine :: (Num a, Ord a, Show a) => [Point2 a] -> [Point2 a]
convexHull2SweepLine [] = []
convexHull2SweepLine [p] = [p]
convexHull2SweepLine [p, q] = [p, q]
convexHull2SweepLine ps =
  -- before anything, we sort the points from left to right
  let (p:q:r:qs) = sortFromLeftToRight ps
  -- then, we find a left turn triangle
      o = orientation2 p q r
      firstPoints = if o == LeftTurn then [p, q, r] else [p, r, q]
  -- finally, we construct incrementally the boundary of the convex hull
   in go firstPoints qs
      where go [] _ = []
            go boundary [] = boundary
            go boundary@(b:bs) (r:rs) =
              let lambda           = findLeftTurn bs b r
                  mu               = findRightTurn bs b r
                  (f1, r1)         = break (== mu) boundary
                  (f2, _)          = break (== lambda) r1 in
              traceShow f1 $ go (r: f2 ++ f1) rs

findLeftTurn :: (Num a, Ord a) => [Point2 a] -> Point2 a -> Point2 a -> Point2 a
findLeftTurn points b q =
    case find (\p -> orientation2 q b p == LeftTurn) points of
        Nothing -> b
        Just r -> r

findRightTurn :: (Num a, Ord a) => [Point2 a] -> Point2 a -> Point2 a -> Point2 a
findRightTurn points b q =
    case find (\p -> orientation2 q b p == RightTurn) points of
        Nothing -> b
        Just r -> r

-- | Finds the point with the lowest y-cooordinate.
lowestY :: (Ord a) => [Point2 a] -> Point2 a
lowestY = head . sortBy (comparing y `mappend` comparing x)

-- | Sorts points from left to right.
sortFromLeftToRight :: (Ord a) => [Point2 a] -> [Point2 a]
sortFromLeftToRight = sortBy (comparing x `mappend` comparing y)
