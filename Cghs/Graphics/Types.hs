-- | Types used during the rendering.
module Cghs.Graphics.Types
where

import Graphics.Rendering.OpenGL

import Cghs.Types.PointVector2
import Cghs.Types.Polygon2
import Cghs.Types.Segment2
import Cghs.Types.Triangle2

-- | A renderable item.
data RenderableItem a = RenderablePoint2 (Point2 a)
                      | RenderableSegment2 (Segment2 a)
                      | RenderableTriangle2 (Triangle2 a)
                      | RenderablePolygon2 (Polygon2 a)
                      deriving Show

-- | A list of (renderable item, color, isSelected).
type RenderableListItem = [(RenderableItem GLfloat, Color3 GLfloat, Bool)]

-- | Select mode.
data SelectMode = PointMode
                | SegmentMode
                deriving Show

