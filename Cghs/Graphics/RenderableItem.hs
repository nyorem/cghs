-- | Functions related to renderable items (used in the viewer).
module Cghs.Graphics.RenderableItem
where

import Graphics.Rendering.OpenGL

import Cghs.Graphics.Types

import Cghs.Types.Circle2
import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.Segment2
import Cghs.Types.Triangle2
import Cghs.Utils

-- | Render an item.
renderItem :: RenderableItem GLfloat -> IO ()

-- Line
renderItem (RenderableLine2 l) = renderItem (RenderableSegment2 s)
    where s = lineToSegment2 l

-- Point
renderItem (RenderablePoint2 p) = renderPrimitive Points $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)

-- Polygon
renderItem (RenderablePolygon2 p) = renderPrimitive Polygon $ do
    mapM_ (\q -> vertex $ (Vertex3 (x q) (y q)  0 :: Vertex3 GLfloat)) p

-- Segment
renderItem (RenderableSegment2 s) = renderPrimitive Lines $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x q) (y q)  0 :: Vertex3 GLfloat)
        where p = src s
              q = dst s

-- Triangle
renderItem (RenderableTriangle2 t) = renderPrimitive Triangles $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x q) (y q)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x r) (y r)  0 :: Vertex3 GLfloat)
        where p = p1 t
              q = p2 t
              r = p3 t

-- Circle
renderItem (RenderableCircle2 (o, r)) = renderItem (RenderablePolygon2 p)
    where p = map Point2 [ (x o + realToFrac r * cos (2 * pi * k / nmax),
                            y o + realToFrac r * sin (2 * pi * k / nmax))
                          | k <- [0 .. nmax - 1]  ]
          nmax = 30

-- | Render a list of items.
renderItemList :: RenderableListItem -> IO ()
renderItemList = mapM_ (\(r, c, isSelected) -> do
        color $ if isSelected then red else c
        renderItem r
    )

-- | Get a point from a renderable.
getRenderablePoint :: RenderableItem a -> Point2 a
getRenderablePoint (RenderablePoint2 p) = p
getRenderablePoint _ = undefined

-- | Get a segment from a renderable.
getRenderableSegment :: RenderableItem a -> Segment2 a
getRenderableSegment (RenderableSegment2 s) = s
getRenderableSegment _ = undefined

-- | Get a line from a renderable.
getRenderableLine :: RenderableItem a -> Line2 a
getRenderableLine (RenderableLine2 l) = l
getRenderableLine _ = undefined

-- | Determines if a renderable is a point.
isRenderablePoint :: RenderableItem a -> Bool
isRenderablePoint (RenderablePoint2 _) = True
isRenderablePoint _ = False

-- | Determines if a renderable is a line.
isRenderableLine :: RenderableItem a -> Bool
isRenderableLine (RenderableLine2 _) = True
isRenderableLine _ = False

-- | Determines if a renderable is a segment.
isRenderableSegment :: RenderableItem a -> Bool
isRenderableSegment (RenderableSegment2 _) = True
isRenderableSegment _ = False

-- | Determines if a renderable is a triangle.
isRenderableTriangle :: RenderableItem a -> Bool
isRenderableTriangle (RenderableTriangle2 _) = True
isRenderableTriangle _ = False

-- | Determines if a renderable is a polygon.
isRenderablePolygon :: RenderableItem a -> Bool
isRenderablePolygon (RenderablePolygon2 _) = True
isRenderablePolygon _ = False

-- | Gives the nature predicate corresponding to the current selection mode.
isRenderable :: SelectMode -> (RenderableItem a -> Bool)
isRenderable PointMode = isRenderablePoint
isRenderable SegmentMode = isRenderableSegment
isRenderable LineMode = isRenderableLine

-- | Gives the intersection predicate corresponding to the current
-- selection mode.
isInCircleRenderable :: (Floating a, Ord a) => SelectMode -> (RenderableItem a -> Circle2 a -> Bool)
isInCircleRenderable PointMode = \r -> isInCircle2 (getRenderablePoint r)
isInCircleRenderable SegmentMode = \r -> doesSegmentIntersectCircle2 (getRenderableSegment r)
isInCircleRenderable LineMode = \r -> doesLineIntersectCircle2 (getRenderableLine r)

-- | Returns all of the selected items.
selectedItems :: RenderableListItem -> RenderableListItem
selectedItems = filter (\(_, _, b) -> b)

-- | Returns all of the non selected items.
nonSelectedItems :: RenderableListItem -> RenderableListItem
nonSelectedItems = filter (\(_, _, b) -> not b)

-- | Gets all of the points in the renderable list.
getPoints :: RenderableListItem -> [Point2 GLfloat]
getPoints = map (\(RenderablePoint2 p) -> p) . filter isRenderablePoint . fst3 . unzip3 . selectedItems

-- | Toggles the select state of an item.
toggleSelectedItem :: (a, b, Bool) -> (a, b, Bool)
toggleSelectedItem (r, c, b) = (r, c, not b)

-- | Toggles the selected component of the items which satisfies the predicate.
toggleSelected :: (RenderableItem GLfloat -> Bool) -> RenderableListItem -> RenderableListItem
toggleSelected p = map (\i@(r, _, _) -> if p r then toggleSelectedItem i else i)

-- | Deselect all the items that satisfies the predicate.
deselectItems :: (RenderableItem GLfloat -> Bool) -> RenderableListItem -> RenderableListItem
deselectItems p = map (\i@(r, c, _) -> if p r then (r, c, False) else i)

-- | Select all the items that satisfies the predicate.
selectItems :: (RenderableItem GLfloat -> Bool) -> RenderableListItem -> RenderableListItem
selectItems p = map (\i@(r, c, _) -> if p r then (r, c, True) else i)

-- Common colors.
-- | Black.
black :: Color3 GLfloat
black = Color3 0 0 0

-- | White.
white :: Color3 GLfloat
white = Color3 255 255 255

-- | Red.
red :: Color3 GLfloat
red = Color3 255 0 0

-- | Green.
green :: Color3 GLfloat
green = Color3 0 255 0

-- | Blue.
blue :: Color3 GLfloat
blue = Color3 0 0 255

