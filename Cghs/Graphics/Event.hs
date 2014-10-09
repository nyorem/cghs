-- | Events handling for the viewer.
module Cghs.Graphics.Event
where

import Control.Lens
import Control.Monad ( when )
import Data.IORef

import qualified Graphics.UI.GLFW as W

import Cghs.Algorithms.ConvexHull2
import Cghs.Algorithms.Triangulation2

import Cghs.Graphics.RenderableItem
import Cghs.Graphics.Types
import Cghs.Graphics.Utils

import Cghs.Types.Segment2
import Cghs.Types.PointVector2
import Cghs.Utils

-- | Map between a key and the function to execute.
--
-- Warning: the keyboard layout is QWERTY
keyEventFunctions :: [(W.Key, IORef ViewerState -> W.Window -> IO ())]
keyEventFunctions =
    [
      -- 'esc' closes the window
      (W.Key'Escape, \_ window -> W.setWindowShouldClose window True)

      -- 'r' reset the renderable list
    , (W.Key'R, \ref _ -> writeIORef ref initialViewerState)

      -- 'c' computes the convex hull of all the points in the list
    , (W.Key'C, \ref _ -> do
        viewerState <- readIORef ref
        let chull = convexHull2 $ getPoints $ viewerState ^. renderList
            newState = viewerState & renderList %~ ((RenderablePolygon2 chull, blue, False) :)
        writeIORef ref newState
      )

      -- 't' computes the triangulation of all the points in the list
      -- or the triangulation of a polygon
      -- depending on the current mode
    , (W.Key'T, \ref _ -> do
        viewerState <- readIORef ref
        case viewerState ^. selectionMode of
            PointMode -> do
                let tri = triangulatePointSet2 $ getPoints $ viewerState ^. renderList
                    newState = viewerState & renderList %~ (++  map (\t -> (RenderableTriangle2 t, green, False)) tri)
                writeIORef ref newState
            PolygonMode -> do
                let polys = getPolygons $ viewerState ^. renderList
                    tris = concat $ map triangulatePolygon2 polys
                    newState = viewerState & renderList %~ (++  map (\t -> (RenderableTriangle2 t, green, False)) tris)
                writeIORef ref newState
            _ -> return ()
      )

      -- 'l' creates a line between two points
    , (W.Key'L, \ref _ -> do
        viewerState <- readIORef ref
        let list = viewerState ^. renderList
        when (length (selectedItems list) == 2) $ do
            let [p, q] = getPoints $ selectedItems list
                l = (RenderableLine2 $ (p, (p .-. q)), blue, False)
                newState = viewerState & renderList %~ (l :)
            writeIORef ref newState
      )

      -- 's' creates a segment between two points
    , (W.Key'S, \ref _ -> do
        viewerState <- readIORef ref
        let list = viewerState ^. renderList
        when (length (selectedItems list) == 2) $ do
            let [p, q] = getPoints $ selectedItems list
                s = (RenderableSegment2 $ Segment2 p q, blue, False)
                newState = viewerState & renderList %~ (s :)
            writeIORef ref newState
      )

      -- 'p' creates a polygon where the vertices are the selected points
    , (W.Key'P, \ref _ -> do
        viewerState <- readIORef ref
        let p = RenderablePolygon2 $ getPoints $ viewerState ^. renderList
            newState = viewerState & renderList %~ ((p, blue, False) :)
        writeIORef ref newState
      )

      -- 'a' selects all the items according to the current selection mode
    , (W.Key'Q, \ref _ -> do
        viewerState <- readIORef ref
        let mode = viewerState ^. selectionMode
            newState = viewerState & renderList %~ toggleSelected (isRenderable mode)
        writeIORef ref newState
      )

      -- 'd' deletes the selected items
    , (W.Key'D, \ref _ -> do
        viewerState <- readIORef ref
        let newState = viewerState & renderList %~ nonSelectedItems
        writeIORef ref newState
      )

      -- 'm' changes the selection mode
    , (W.Key'Semicolon, \ref _ -> do
        viewerState <- readIORef ref
        let newState' = viewerState & selectionMode %~ succ'
            newState = newState' & renderList %~ deselectItems (const True)
        writeIORef ref newState
      )
    ]

-- | Map between a mouse button and the function to execute.
mouseEventFunctions :: [(W.MouseButton, IORef ViewerState -> W.Window -> IO ())]
mouseEventFunctions =
    [
      -- left click adds a point
      (W.MouseButton'1, \ref window -> do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        viewerState <- readIORef ref
        let p = RenderablePoint2 $ Point2 (xMouse, yMouse)
            newState = viewerState & renderList %~ ((p, white, False) :)
        writeIORef ref newState
      )

      -- right clicks selects items according to the current selection mode
    , (W.MouseButton'2, \ref window -> do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        viewerState <- readIORef ref
        let mode = viewerState ^. selectionMode
            selectItem m p r@(o, _, _) = if isRenderable m o && isInCircleRenderable m o (p, 0.1) then toggleSelectedItem r else r
            newState = viewerState & renderList %~ map (selectItem mode $ Point2 (xMouse, yMouse))
        writeIORef ref newState
      )
    ]

