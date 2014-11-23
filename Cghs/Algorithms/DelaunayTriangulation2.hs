-- | Delaunay triangulations in two dimensions.
module Cghs.Algorithms.DelaunayTriangulation2 ( delaunayTriangulation2 )
where

import Data.List ( nub )

import Cghs.Algorithms.Triangulation2
import Cghs.Types.PointVector2
import Cghs.Types.Segment2
import Cghs.Types.Triangle2
import Cghs.Utils ( deleteList )

-- | A Delaunay edge contains two points and
-- a boolean to know if the Edge is locally Delone or not.
data DelaunayEdge a = DelaunayEdge (Point2 a) (Point2 a) Bool

-- | Flips an edge.
flipEdge :: DelaunayEdge a -> [Point2 a] -> [Triangle2 a]
flipEdge (DelaunayEdge a b _) points =
    case points of
        [c] -> [Triangle2 a b c]
        [c, d] -> [Triangle2 b c d, Triangle2 a c d]
        _ -> error "flipEdge: too much triangles"

-- | Is an edge locally Delone or not?
isLocallyDelone :: (Eq a, Fractional a, Floating a, Real a) => Segment2 a -> [Point2 a] -> Bool
isLocallyDelone (Segment2 a b) points =
    case points of
        [_] -> True
        [c, d] -> not $ isInsideCircumcircle2 (Triangle2 a b c) d
        _ -> error "isLocallyDelone: too much triangles"

-- | Auxiliary function used by the Delaunay triangulation algorithm.
delaunayTriangulation2' :: (RealFloat a) => [Triangle2 a] -> [DelaunayEdge a] -> [Triangle2 a]
delaunayTriangulation2' tri [] = tri
delaunayTriangulation2' tri (edge@(DelaunayEdge a b _):edges) =
    let t = adjacentTriangles2 tri (Segment2 a b)
        points = otherPoints t $ Segment2 a b
        flippedTriangles = flipEdge edge points
        tri' = deleteList t tri ++ flippedTriangles
        nonDeloneEdges = map (\(Segment2 p q) -> DelaunayEdge p q False) $ filter (\s -> not $ isLocallyDelone s (otherPoints flippedTriangles s)) $ concatMap triToEdges flippedTriangles
        edges' = nonDeloneEdges ++ edges
    in delaunayTriangulation2' tri' edges'
    where triToEdges t = [Segment2 (p1 t) (p2 t),
                          Segment2 (p1 t) (p3 t),
                          Segment2 (p2 t) (p3 t)]
          otherPoints t' (Segment2 c d) = filter (\p -> p /= c && p /= d) . nub . concatMap (\(Segment2 p q) -> [p, q]) $ concatMap triToEdges t'

-- | Delaunay triangulation of a point set
-- using a 'flip' algorithm.
delaunayTriangulation2 :: (RealFloat a) => [Point2 a] -> [Triangle2 a]
delaunayTriangulation2 ps = delaunayTriangulation2' tri initialEdges
    where tri = triangulatePointSet2 ps
          initialEdges = map (\(Segment2 p q) -> DelaunayEdge p q True) $ edgesTriangulation2 tri

