module Main where

import Control.Monad ( when )
import Data.IORef
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL
import Cghs.Graphics.RenderableItem
import Cghs.Graphics.OGLUtils
import Cghs.Graphics.Types

import Cghs.Algorithms.ConvexHull2
import Cghs.Algorithms.Triangulation2

import Cghs.Types.Circle2
import Cghs.Types.Segment2
import Cghs.Types.PointVector2
import Cghs.Utils

width, height :: Int
width = 800
height = 600

errorCallBack :: W.ErrorCallback
errorCallBack _ desc = hPutStrLn stderr desc

-- Warning: the keyboard layout is QWERTY
keyCallback :: IORef RenderableListItem -> W.KeyCallback
keyCallback ref window key _ action _ = do
    -- 'esc' closes the window
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

    -- 'r' reset the renderable list
    when (key == W.Key'R && action == W.KeyState'Pressed) $ do
        writeIORef ref []

    -- 'c' computes the convex hull of all the points in the list
    when (key == W.Key'C && action == W.KeyState'Pressed) $ do
        list <- readIORef ref
        let points = map (\(RenderablePoint2 p) -> p) . filter isPoint . fst3 . unzip3 $ selectedItems list
            chull = convexHull2 points
        modifyIORef ref $ ((RenderablePolygon2 chull, blue, False) :)

    -- 't' computes the triangulation of all the points in the list
    when (key == W.Key'T && action == W.KeyState'Pressed) $ do
        list <- readIORef ref
        let points = map (\(RenderablePoint2 p) -> p) . filter isPoint . fst3 . unzip3 $ selectedItems list
            tri = triangulatePointSet2 points
        modifyIORef ref $ (++  map (\t -> (RenderableTriangle2 t, green, False)) tri)

    -- 'l' creates a line between two points
    when (key == W.Key'L && action == W.KeyState'Pressed) $ do
        list <- readIORef ref
        when (length list == 2) $ do
            let [p, q] = map (\(RenderablePoint2 p) -> p) . filter isPoint . fst3 . unzip3 $ selectedItems list
                l = (RenderableLine2 $ (p, (p .-. q)), blue, False)
            modifyIORef ref $ (l :)

    -- 's' creates a segment between two points
    when (key == W.Key'S && action == W.KeyState'Pressed) $ do
        list <- readIORef ref
        when (length list == 2) $ do
            let [p, q] = map (\(RenderablePoint2 p) -> p) . filter isPoint . fst3 . unzip3 $ selectedItems list
                s = (RenderableSegment2 $ Segment2 p q, blue, False)
            modifyIORef ref $ (s :)

    -- 'a' selects all the points
    when (key == W.Key'Q && action == W.KeyState'Pressed) $ do
        modifyIORef ref $ toggleSelected isPoint

    -- 'd' deletes the selected points
    when (key == W.Key'D && action == W.KeyState'Pressed) $ do
        modifyIORef ref $ nonSelectedItems

mouseButtonCallback :: IORef RenderableListItem -> W.MouseButtonCallback
mouseButtonCallback ref window button state _ = do
    -- left click adds a point
    when (button == W.MouseButton'1 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        let p = RenderablePoint2 $ Point2 (xMouse, yMouse)
        modifyIORef ref ((p, white, False) :)

    -- right clicks selects points
    when (button == W.MouseButton'2 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- getCursorPosConverted window width height
        modifyIORef ref $ map (selectPoint $ Point2 (xMouse, yMouse))
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
            let renderList = []
            renderRef <- newIORef renderList
            W.setKeyCallback window (Just $ keyCallback renderRef)
            W.setMouseButtonCallback window (Just $ mouseButtonCallback renderRef)

            initGLParams
            mainLoop renderRef window

            W.destroyWindow window
            W.terminate
            exitSuccess

mainLoop :: IORef RenderableListItem -> W.Window -> IO ()
mainLoop ref window = unless' (W.windowShouldClose window) $ do
        clear [ColorBuffer]
        list <- readIORef ref
        renderItemList list

        W.pollEvents
        W.swapBuffers window

        mainLoop ref window

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

