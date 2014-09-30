{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad ( when )
import Data.IORef
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL
import Cghs.Graphics.RenderableItem
import Cghs.Graphics.Utils
import Cghs.Graphics.Types

import Cghs.Algorithms.ConvexHull2
import Cghs.Algorithms.Triangulation2

import Cghs.Types.Circle2
import Cghs.Types.Segment2
import Cghs.Types.PointVector2
import Cghs.Utils

import Control.Lens

data ViewerState = ViewerState
                 { _renderList :: RenderableListItem,
                   _selectionMode :: SelectMode
                 }
makeLenses ''ViewerState

initialViewerState :: ViewerState
initialViewerState = ViewerState { _renderList = [], _selectionMode = PointMode }

width, height :: Int
width = 800
height = 600

errorCallBack :: W.ErrorCallback
errorCallBack _ desc = hPutStrLn stderr desc

-- Warning: the keyboard layout is QWERTY
keyCallback :: IORef ViewerState -> W.KeyCallback
keyCallback ref window key _ action _ = do
    -- 'esc' closes the window
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

    -- 'r' reset the renderable list
    when (key == W.Key'R && action == W.KeyState'Pressed) $ do
        writeIORef ref initialViewerState

    -- 'c' computes the convex hull of all the points in the list
    when (key == W.Key'C && action == W.KeyState'Pressed) $ do
        viewerState <- readIORef ref
        let chull = convexHull2 $ getPoints $ viewerState ^. renderList
            newState = viewerState & renderList %~ ((RenderablePolygon2 chull, blue, False) :)
        writeIORef ref newState

    -- 't' computes the triangulation of all the points in the list
    when (key == W.Key'T && action == W.KeyState'Pressed) $ do
        viewerState <- readIORef ref
        let tri = triangulatePointSet2 $ getPoints $ viewerState ^. renderList
            newState = viewerState & renderList %~ (++  map (\t -> (RenderableTriangle2 t, green, False)) tri)
        writeIORef ref newState

    -- 'l' creates a line between two points
    when (key == W.Key'L && action == W.KeyState'Pressed) $ do
        viewerState <- readIORef ref
        let list = viewerState ^. renderList
        when (length (selectedItems list) == 2) $ do
            let [p, q] = getPoints list
                l = (RenderableLine2 $ (p, (p .-. q)), blue, False)
                newState = viewerState & renderList %~ (l :)
            writeIORef ref newState

    -- -- 's' creates a segment between two points
    when (key == W.Key'S && action == W.KeyState'Pressed) $ do
        viewerState <- readIORef ref
        let list = viewerState ^. renderList
        when (length (selectedItems list) == 2) $ do
            let [p, q] = getPoints list
                s = (RenderableSegment2 $ Segment2 p q, blue, False)
                newState = viewerState & renderList %~ (s :)
            writeIORef ref newState

    -- 'a' selects all the points
    when (key == W.Key'Q && action == W.KeyState'Pressed) $ do
        viewerState <- readIORef ref
        let newState = viewerState & renderList %~ toggleSelected isPoint
        writeIORef ref newState

    -- 'd' deletes the selected points
    when (key == W.Key'D && action == W.KeyState'Pressed) $ do
        viewerState <- readIORef ref
        let newState = viewerState & renderList %~ nonSelectedItems
        writeIORef ref newState

mouseButtonCallback :: IORef ViewerState -> W.MouseButtonCallback
mouseButtonCallback ref window button state _ = do
    -- left click adds a point
    when (button == W.MouseButton'1 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        viewerState <- readIORef ref
        let p = RenderablePoint2 $ Point2 (xMouse, yMouse)
            newState = viewerState & renderList %~ ((p, white, False) :)
        writeIORef ref newState

    -- right clicks selects points
    when (button == W.MouseButton'2 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        viewerState <- readIORef ref
        let newState = viewerState & renderList %~ map (selectPoint $ Point2 (xMouse, yMouse))
        writeIORef ref newState
        where selectPoint p r@((RenderablePoint2 o), c, b) = if isInCircle2 (p, 0.1) o then (RenderablePoint2 o, c, not b) else r
              selectPoint _ r = r

main :: IO ()
main = do
    W.setErrorCallback (Just errorCallBack)
    successfulInit <- W.init

    bool successfulInit exitFailure $ do
        W.windowHint $ W.WindowHint'ContextVersionMajor 2
        W.windowHint $ W.WindowHint'ContextVersionMinor 1
        W.windowHint $ W.WindowHint'Resizable False

        mw <- W.createWindow width height "cghs" Nothing Nothing
        maybe' mw (W.terminate >> exitFailure) $ \window -> do
            W.makeContextCurrent mw
            stateRef <- newIORef initialViewerState
            W.setKeyCallback window (Just $ keyCallback stateRef)
            W.setMouseButtonCallback window (Just $ mouseButtonCallback stateRef)

            initGLParams
            mainLoop stateRef window

            W.destroyWindow window
            W.terminate
            exitSuccess

mainLoop :: IORef ViewerState -> W.Window -> IO ()
mainLoop ref window = unless' (W.windowShouldClose window) $ do
        clear [ColorBuffer]
        viewerState <- readIORef ref
        -- TODO
        changeTitle viewerState window
        renderItemList $ viewerState ^. renderList

        W.pollEvents
        W.swapBuffers window

        mainLoop ref window

changeTitle :: ViewerState -> W.Window -> IO ()
changeTitle state w = do
    let mode = state ^. selectionMode
    W.setWindowTitle w $ "cghs - " ++ show mode

