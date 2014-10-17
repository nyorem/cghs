-- | Events handling for the viewer.
module Cghs.Graphics.Event
where

import Control.Lens
import Control.Monad ( when )
import Data.Either ( lefts, rights )
import Data.IORef

import qualified Graphics.UI.GLFW as W

import Cghs.Algorithms.ConvexHull2
import Cghs.Algorithms.Triangulation2
import Cghs.Algorithms.VoronoiDiagram2

import Cghs.Graphics.RenderableItem
import Cghs.Graphics.Types
import Cghs.Graphics.Utils

import Cghs.Types.Segment2
import Cghs.Types.PointVector2
import Cghs.Utils

-- | Map between a key and the function to execute.
--
-- Warning: the keyboard layout is QWERTY
keyEventFunctions :: [(W.Key, IORef ViewerState -> W.ModifierKeys -> W.Window -> IO ())]
keyEventFunctions =
    [
      -- 'esc' closes the window
      (W.Key'Escape, \_ _ window -> W.setWindowShouldClose window True)

      -- 'r' reset the renderable list
    , (W.Key'R, \ref _ _ -> writeIORef ref initialViewerState)

      -- 'c' computes the convex hull of all the points in the list
    , (W.Key'C, \ref _ _ -> do
        modifyIORef ref $ \viewerState ->
            let chull = convexHull2 $ getPoints $ viewerState ^. renderList
            in viewerState & renderList %~ ((RenderablePolygon2 chull, blue, False) :)
      )

      -- 't' computes the triangulation of all the points in the list
      -- or the triangulation of a polygon
      -- depending on the current mode
    , (W.Key'T, \ref _ _ -> do
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

      -- 'v' computes the Voronoi diagram of all the points in the list
    , (W.Key'V, \ref _ _ -> do
        modifyIORef ref $ \viewerState ->
            let diagram = map snd $ voronoiDiagram2 $ getPoints $ viewerState ^. renderList
                polys = map (\p -> (RenderablePolygon2 p, orange, False)) $ lefts diagram
                regions = map (\r -> (RenderablePlaneRegion2 r, orange, False)) $ rights diagram
            in viewerState & renderList %~ (++ (polys ++ regions))
      )

      -- 'l' creates a line between two points
    , (W.Key'L, \ref _ _ -> do
        viewerState <- readIORef ref
        let list = viewerState ^. renderList
        when (length (selectedItems list) == 2) $ do
            let [p, q] = getPoints $ selectedItems list
                l = (RenderableLine2 $ (p, (p .-. q)), blue, False)
                newState = viewerState & renderList %~ (l :)
            writeIORef ref newState
      )

      -- 's' creates a segment between two points
    , (W.Key'S, \ref _ _ -> do
        viewerState <- readIORef ref
        let list = viewerState ^. renderList
        when (length (selectedItems list) == 2) $ do
            let [p, q] = getPoints $ selectedItems list
                s = (RenderableSegment2 $ Segment2 p q, blue, False)
                newState = viewerState & renderList %~ (s :)
            writeIORef ref newState
      )

      -- 'p' creates a polygon where the vertices are the selected points
    , (W.Key'P, \ref _ _ -> do
        modifyIORef ref $ \viewerState ->
            let p = RenderablePolygon2 $ getPoints $ viewerState ^. renderList
            in viewerState & renderList %~ ((p, blue, False) :)
      )

      -- 'a' selects all the items according to the current selection mode
    , (W.Key'Q, \ref _ _ -> do
        modifyIORef ref $ \viewerState ->
            let mode = viewerState ^. selectionMode
            in viewerState & renderList %~ toggleSelected (isRenderable mode)
      )

      -- 'd' deletes the selected items
    , (W.Key'D, \ref _ _ -> do
        modifyIORef ref $ \viewerState -> viewerState & renderList %~ nonSelectedItems
      )

      -- 'm' chooses the next selection mdoe
      -- 'M' chooses the previous selection mdoe
    , (W.Key'Semicolon, \ref mods _ -> do
        modifyIORef ref $ \viewerState ->
            let newState' = viewerState & selectionMode %~ if W.modifierKeysShift mods then pred' else succ'
            in newState' & renderList %~ deselectItems (const True)
      )
    ]

-- | Map between a mouse button and the function to execute.
mouseEventFunctions :: [(W.MouseButton, IORef ViewerState -> W.Window -> IO ())]
mouseEventFunctions =
    [
      -- left click adds a point
      (W.MouseButton'1, \ref window -> do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        modifyIORef ref $ \viewerState ->
            let p = RenderablePoint2 $ Point2 (xMouse, yMouse)
            in viewerState & renderList %~ ((p, white, False) :)
      )

      -- right clicks selects items according to the current selection mode
    , (W.MouseButton'2, \ref window -> do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        modifyIORef ref $ \viewerState ->
            let mode = viewerState ^. selectionMode
                selectItem m p r@(o, _, _) = if isRenderable m o && isInCircleRenderable m o (p, 0.1) then toggleSelectedItem r else r
            in viewerState & renderList %~ map (selectItem mode $ Point2 (xMouse, yMouse))
      )
    ]

