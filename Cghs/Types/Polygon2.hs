-- | Polygons in two dimensions.
module Cghs.Types.Polygon2
where

import Data.List ( elemIndex )
import Data.Maybe ( fromJust, isJust )

import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.Segment2
import Cghs.Types.Triangle2

import Cghs.Utils

-- | We consider a polygon to be a list of points: its vertices
-- are given in counter-clockwise order.
type Polygon2 a = [Point2 a]

-- | Type of a vertex in a polygon.
data VertexType = ConvexVertex | ConcaveVertex deriving (Eq, Show)

-- | Determines the type of a vertex in a polygon.
typeVertexPolygon2 :: (Fractional a, Ord a) => Point2 a -> Polygon2 a -> VertexType
typeVertexPolygon2 p poly
    | area > 0 = ConvexVertex
    | otherwise = ConcaveVertex
        where (pm1, pp1) = (previousVertexPolygon2 p poly, nextVertexPolygon2 p poly)
              tri = Triangle2 pm1 p pp1
              area = signedAreaTriangle2 tri

-- | Determines if a vertex of a polygon is convex.
isConvexVertexPolygon2 :: (Fractional a, Ord a) => Point2 a -> Polygon2 a -> Bool
isConvexVertexPolygon2 p poly = typeVertexPolygon2 p poly == ConvexVertex

-- | Determines if a vertex of a polygon is concave.
isConcaveVertexPolygon2 ::(Fractional a, Ord a) =>  Point2 a -> Polygon2 a -> Bool
isConcaveVertexPolygon2 p poly = typeVertexPolygon2 p poly == ConcaveVertex

-- | Computes the area of a simple polygon.
areaPolygon2 :: (Fractional a) => Polygon2 a -> a
areaPolygon2 poly = abs $ (foldr step 0 edges) / 2
    where edges = polygonEdges2 poly
          step (p, q) acc = acc + (x p * y q - x q * y p)

-- | Computes the centroid of a simple polygon.
centroidPolygon2 :: (Fractional a) => Polygon2 a -> Point2 a
centroidPolygon2 poly = Point2 (cx, cy)
    where edges = polygonEdges2 poly
          area = areaPolygon2 poly
          cx = (foldr stepx 0 edges) / (6 * area)
          cy = (foldr stepy 0 edges) / (6 * area)
          stepx (p, q) acc = acc + (x p + x q) * (x p * y q - x q * y p)
          stepy (p, q) acc = acc + (y p + y q) * (x p * y q - x q * y p)

-- | Gets the edges of a polygon.
polygonEdges2 :: Polygon2 a -> [(Point2 a, Point2 a)]
polygonEdges2 [] = []
polygonEdges2 poly@(p0:ps) = zip poly (ps ++ [p0])

-- | Gets the next vertex in a polygon.
nextVertexPolygon2 :: (Eq a) => Point2 a -> Polygon2 a -> Point2 a
nextVertexPolygon2 v p = p <!!> (i + 1)
    where i = fromJust (elemIndex v p)

-- | Gets the previous vertex in a polygon.
previousVertexPolygon2 :: (Eq a) => Point2 a -> Polygon2 a -> Point2 a
previousVertexPolygon2 v p = p <!!> (i - 1)
    where i = fromJust (elemIndex v p)

-- | Determines if a point is inside a polygon.
isInsidePolygon2 :: (Fractional a, Ord a) => Polygon2 a -> Point2 a -> Bool
isInsidePolygon2 [] _ = False
isInsidePolygon2 poly point = foldr step False couples
    where couples = zip poly (last poly : init poly)
          step (p, q) c =
              if (y p > y point) /= (y q > y point) &&
                 (x point < x p + ((x q - x p) * (y point - y p)) / (y q - y p) )
              then not c
              else c

-- | Reverses the order of a polygon.
reverseOrderPolygon2 :: Polygon2 a -> Polygon2 a
reverseOrderPolygon2 [] = []
reverseOrderPolygon2 (p:ps) = p : reverse ps

-- | Computes the intersection of a polygon and a line.
intersectPolygonLine2 :: (Eq a, Fractional a) => Polygon2 a -> Line2 a -> Maybe [Point2 a]
intersectPolygonLine2 poly l
    | null inter = Nothing
    | otherwise = sequence inter
    where edges = map (\(p, q) -> segmentToLine2 $ Segment2 p q) $ polygonEdges2 poly
          inter = filter isJust $ map (intersectLines2 l) edges

