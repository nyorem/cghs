-- | Functions related to renderable items (used in the viewer).
module Graphics.RenderableItem
where

import Graphics.Rendering.OpenGL

import Math.Types.PointVector2
import Math.Types.Segment2
import Math.Types.Triangle2

-- | A renderable item.
data RenderableItem a = RenderablePoint2 (Point2 a)
                      | RenderableSegment2 (Segment2 a)
                      | RenderableTriangle2 (Triangle2 a)
                      deriving Show

-- | A list of (renderable item, color).
type RenderableListItem = [(RenderableItem GLfloat, Color3 GLfloat)]

-- | Render an item.
renderItem :: RenderableItem GLfloat -> IO ()

renderItem (RenderablePoint2 p) = renderPrimitive Points $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)

renderItem (RenderableSegment2 s) = renderPrimitive Lines $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x q) (y q)  0 :: Vertex3 GLfloat)
        where p = src s
              q = dst s

renderItem (RenderableTriangle2 t) = renderPrimitive Triangles $ do
    vertex $ (Vertex3 (x p) (y p)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x q) (y q)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x r) (y r)  0 :: Vertex3 GLfloat)
        where p = p1 t
              q = p2 t
              r = p3 t

-- | Render a list of items.
renderItemList :: RenderableListItem -> IO ()
renderItemList = mapM_ (\(r, c) -> do
        color c
        renderItem r
    )

-- Common colors
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

