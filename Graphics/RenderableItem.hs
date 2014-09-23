module Graphics.RenderableItem
where

import Graphics.Rendering.OpenGL

import Math.Types.PointVector2
import Math.Types.Segment2
import Math.Types.Triangle2

data RenderableItem a = RenderablePoint2 (Point2 a)
                      | RenderableSegment2 (Segment2 a)
                      | RenderableTriangle2 (Triangle2 a)
                      deriving Show
type RenderableListItem = [RenderableItem GLfloat]

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

renderItemList :: RenderableListItem -> IO ()
renderItemList = mapM_ renderItem

