-- | Voronoi diagrams in two dimensions.
module Cghs.Algorithms.VoronoiDiagram2 ( voronoiDiagram2 )
where

import Data.Maybe ( fromJust )

import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.PlaneRegion2
import Cghs.Types.Polygon2
import Cghs.Types.Ray2
import Cghs.Types.Segment2

-- | A Voronoi cell is either a convex region (polygon) or
-- a non bounded region (plane region) with a seed.
type VoronoiCell2 a = (Point2 a, Either (Polygon2 a) (PlaneRegion2 a))

-- TODO
-- | Computes the Voronoi diagram of a point set
-- using the Green-Sibson algorithm.
voronoiDiagram2 :: (Fractional a, Ord a) => [Point2 a] -> [VoronoiCell2 a]
voronoiDiagram2 points
    | len < 2 = []
    | len == 2 =
        let [p, q] = points
            s = Segment2 p q
            (mid, middir) = bisector2 s
            upRay = Ray2 mid middir
            downRay = Ray2 mid (negateV middir) in
        [(p, Right $ PlaneRegion2 upRay [mid, mid] downRay)] ++
        [(q, Right $ PlaneRegion2 upRay [mid, mid] downRay)]
    | otherwise =
        let (p:ps) = points
            diagram' = voronoiDiagram2 ps
            cell@(q, reg) = findCellContaining p diagram'
            med = bisector2 $ Segment2 p q
            [p1, p2] = fromJust $ intersectVoronoiCellLine2 cell med
        in
            -- TODO
            case reg of
                Right (PlaneRegion2 l b r) -> cell : diagram'
                Left poly -> let (f1, rest1) = break (== p1) poly
                                 (f2, rest2) = break (== p2) rest1
                             in [(q, Left f2)] ++ [(p, Left (rest2 ++ f1))] ++ diagram'
        where len = length points

-- | Finds the Voronoi cell containing a point,
-- returns the seed of the found cell.
findCellContaining :: (Fractional a, Ord a) => Point2 a -> [VoronoiCell2 a] -> VoronoiCell2 a
findCellContaining _ [] = undefined
findCellContaining p (c@(q, Left poly):cs) =
    if isInsidePolygon2 poly p then c
    else findCellContaining p cs
findCellContaining p (c@(q, Right reg):cs) =
    if isInsidePlaneRegion2 p reg then c
    else findCellContaining p cs

-- | Computes the intersection of a Voronoi cell and a line.
intersectVoronoiCellLine2 :: (Eq a, Fractional a) => VoronoiCell2 a -> Line2 a -> Maybe [Point2 a]
intersectVoronoiCellLine2 (_, Left poly) l = intersectPolygonLine2 poly l
intersectVoronoiCellLine2 (_, Right reg) l = intersectPlaneRegionLine2 reg l

