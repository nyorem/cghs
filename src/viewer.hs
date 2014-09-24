module Main where

import Control.Monad ( unless, when )
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as W
import System.Exit
import System.IO

import Data.IORef
import Math.Utils.Tuple
import Math.Types.Circle2
import Math.Types.PointVector2
import Graphics.RenderableItem
import Math.Algorithms.ConvexHull2

width, height :: Int
width = 800
height = 600

convertToGLFrame :: Int -> Int -> (Double, Double) -> (GLfloat, GLfloat)
convertToGLFrame w h (xMouse, yMouse) = (realToFrac x', realToFrac y')
    where x' =  2 * (xMouse / fromIntegral w) - 1
          y' = -2 * (yMouse / fromIntegral h) + 1

errorCallBack :: W.ErrorCallback
errorCallBack _ desc = hPutStrLn stderr desc

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
        let chull = convexHull2 . map (\(RenderablePoint2 p) -> p) . filter isPoint . fst3 . unzip3 $ selectedItems list
        modifyIORef ref $ ((RenderablePolygon2 chull, blue, False) :)

mouseButtonCallback :: IORef RenderableListItem -> W.MouseButtonCallback
mouseButtonCallback ref window button state _ = do
    -- left click adds a point
    when (button == W.MouseButton'1 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- W.getCursorPos window
        let p = RenderablePoint2 $ Point2 $ convertToGLFrame width height (xMouse, yMouse)
        modifyIORef ref ((p, white, False) :)

    -- right clicks selects points
    when (button == W.MouseButton'2 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- W.getCursorPos window
        list <- readIORef ref
        let filteredList = map (selectPoint $ Point2 $ convertToGLFrame width height (xMouse, yMouse)) list
        writeIORef ref filteredList
        where selectPoint p r@((RenderablePoint2 o), c, b) = if isInCircle (p, 0.1) o then (RenderablePoint2 o, c, not b) else r
              selectPoint _ r = r

initGL :: IO ()
initGL = do
    clearColor $= Color4 0 0 0 0
    pointSize $= 3.0
    polygonMode $= (Line, Line)

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

            initGL
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

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = maybe nothingRes f m

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

