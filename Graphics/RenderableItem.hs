-- | Functions related to renderable items (used in the viewer).
module Graphics.RenderableItem
where

import Graphics.Rendering.OpenGL

import Math.Types.PointVector2
import Math.Types.Polygon2
import Math.Types.Segment2
import Math.Types.Triangle2

-- | A renderable item.
data RenderableItem a = RenderablePoint2 (Point2 a)
                      | RenderableSegment2 (Segment2 a)
                      | RenderableTriangle2 (Triangle2 a)
                      | RenderablePolygon2 (Polygon2 a)
                      deriving Show

-- | A list of (renderable item, color, isSelected).
type RenderableListItem = [(RenderableItem GLfloat, Color3 GLfloat, Bool)]

-- | Render an item.
renderItem :: RenderableItem GLfloat -> IO ()

-- Point
renderItem (RenderablePoint2 p) = renderPrimitive Points $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)

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

-- Polygon
renderItem (RenderablePolygon2 p) = renderPrimitive Polygon $ do
    mapM_ (\q -> vertex $ (Vertex3 (x q) (y q)  0 :: Vertex3 GLfloat)) p

-- | Render a list of items.
renderItemList :: RenderableListItem -> IO ()
renderItemList = mapM_ (\(r, c, isSelected) -> do
        color $ if isSelected then red else c
        renderItem r
    )

-- | Determines if a renderable is a point?
isPoint :: RenderableItem a -> Bool
isPoint (RenderablePoint2 _) = True
isPoint _ = False

-- | Returns all of the selected items.
selectedItems :: RenderableListItem -> RenderableListItem
selectedItems = filter (\(_, _, b) -> b)

-- | Returns all of the non selected items.
nonSelectedItems :: RenderableListItem -> RenderableListItem
nonSelectedItems = filter (\(_, _, b) -> not b)

-- | Toggles the selected component of the items which satisfies the predicate.
toggleSelected :: (RenderableItem GLfloat -> Bool) -> RenderableListItem -> RenderableListItem
toggleSelected p = map (\i@(r, c, b) -> if p r then (r, c, not b) else i)

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

