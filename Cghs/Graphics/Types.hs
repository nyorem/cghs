{-# LANGUAGE TemplateHaskell #-}

-- | Types used during the rendering.
module Cghs.Graphics.Types
where

import Graphics.Rendering.OpenGL

import Cghs.Types.Circle2
import Cghs.Types.Line2
import Cghs.Types.PointVector2
import Cghs.Types.Polygon2
import Cghs.Types.Segment2
import Cghs.Types.Triangle2

import Control.Lens

-- | A renderable item.
data RenderableItem a = RenderablePoint2 (Point2 a)
                      | RenderableLine2 (Line2 a)
                      | RenderableSegment2 (Segment2 a)
                      | RenderableTriangle2 (Triangle2 a)
                      | RenderablePolygon2 (Polygon2 a)
                      | RenderableCircle2 (Circle2 a)
                      deriving Show

-- | A list of (renderable item, color, isSelected).
type RenderableListItem = [(RenderableItem GLfloat, Color3 GLfloat, Bool)]

-- | Select mode.
data SelectMode = PointMode
                | SegmentMode
                deriving (Bounded, Enum, Eq)

instance Show SelectMode where
    show PointMode = "points"
    show SegmentMode = "segments"

-- | State of the viewer.
data ViewerState = ViewerState
                 { _renderList :: RenderableListItem,
                   _selectionMode :: SelectMode
                 }
makeLenses ''ViewerState

-- | Initial state for the viewer.
initialViewerState :: ViewerState
initialViewerState = ViewerState { _renderList = [], _selectionMode = PointMode }

-- | The width of the window.
width :: Int
width = 800

-- | The height of the window.
height :: Int
height = 600

