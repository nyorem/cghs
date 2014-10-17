-- | Voronoi diagrams in two dimensions.
module Cghs.Algorithms.VoronoiDiagram2 ( voronoiDiagram2 )
where

import Cghs.Types.PointVector2
import Cghs.Types.PlaneRegion2
import Cghs.Types.Polygon2
import Cghs.Types.Ray2
import Cghs.Types.Segment2

-- | A Voronoi cell is either a convex region (polygon) or
-- a non bounded region (plane region).
type VoronoiCell2 a = Either (Polygon2 a) (PlaneRegion2 a)

-- TODO
-- | Computes the Voronoi diagram of a point set.
voronoiDiagram2 :: (Fractional a) => [Point2 a] -> [(Point2 a, VoronoiCell2 a)]
voronoiDiagram2 points
    | len < 2 = []
    | len == 2 =
        let [p, q] = points
            s = Segment2 p q
            (mid, middir) = bisector2 s
            upRay = Ray2 mid middir
            downRay = Ray2 mid (negateV middir) in
        [(p, Right $ PlaneRegion2 upRay [mid, mid] downRay)] ++
        [(p, Right $ PlaneRegion2 upRay [mid, mid] downRay)]
    | otherwise =
        let (p:ps) = points
            diagram' = voronoiDiagram2 ps
            cell = findCellContaining p diagram'
        in undefined
        where len = length points

-- | Finds the Voronoi cell containing a point,
-- returns the seed of the found cell.
findCellContaining :: Point2 a -> [(Point2 a, VoronoiCell2 a)] -> Point2 a
findCellContaining _ [] = undefined
findCellContaining p (c:cs) = undefined

