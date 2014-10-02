-- | Triangulations in two dimensions.
module Cghs.Algorithms.Triangulation2 ( triangulatePointSet2, triangulatePolygon2 )
where

import Data.List ( delete, elemIndex )

import Cghs.Types.PointVector2
import Cghs.Types.Polygon2
import Cghs.Types.Triangle2

import Cghs.Utils

-- TODO: debug

-- | Triangulation of a polygon.
triangulatePolygon2 :: (Eq a, Fractional a, Ord a) => Polygon2 a -> [Triangle2 a]
triangulatePolygon2 poly
    | length poly == 3 =
        let [p, q, r] = poly in
        [Triangle2 p q r]
    | otherwise =
        let [p, q, r] = findEarPolygon2 poly
            tri = Triangle2 p q r
            newPoly = delete q poly in
            tri : triangulatePolygon2 newPoly

-- | Finds an ear in a polygon.
findEarPolygon2 :: (Eq a, Fractional a, Ord a) => Polygon2 a -> [Point2 a]
findEarPolygon2 [] = []
findEarPolygon2 (p:ps)
    | b = xs
    | otherwise = findEarPolygon2 ps
        where (b, xs) = isEarPolygon2 (p:ps) p

-- | Determines if a vertex is an ear of a polygon.
isEarPolygon2 :: (Eq a, Fractional a, Ord a) => Polygon2 a -> Point2 a -> (Bool, [Point2 a])
isEarPolygon2 points p =
    case elemIndex p points of
        Nothing -> (False, [])
        Just i ->
            let (pm1, pp1) = (points <!!> (i - 1), points <!!> (i + 1))
                newPoints = deleteList points [pm1, p, pp1]
                tri = Triangle2 pm1 p pp1 in
            (all (not . isInsideTriangle2 tri) newPoints, [pm1, p, pp1])


-- | Triangulation of a point set.
triangulatePointSet2 :: (Num a) => [Point2 a] -> [Triangle2 a]
triangulatePointSet2 ps = undefined

