module Main where

import Control.Monad ( unless, when )
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as W
import System.Exit
import System.IO

import Data.IORef

import Math.Types.PointVector2
import Graphics.RenderableItem

width, height :: Int
width = 800
height = 600

convertToGLFrame :: Int -> Int -> (Double, Double) -> (GLfloat, GLfloat)
convertToGLFrame w h (xMouse, yMouse) = (realToFrac x', realToFrac y')
    where x' = 2 * (xMouse / fromIntegral w) - 1
          y' = -2 * (yMouse / fromIntegral h) + 1

errorCallBack :: W.ErrorCallback
errorCallBack _ desc = hPutStrLn stderr desc

keyCallback :: W.KeyCallback
keyCallback window key _ action _ =
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

mouseButtonCallback :: IORef RenderableListItem -> W.MouseButtonCallback
mouseButtonCallback ref window button state _ =
    when (button == W.MouseButton'1 && state == W.MouseButtonState'Pressed) $ do
        (xMouse, yMouse) <- W.getCursorPos window
        let p = RenderablePoint2 $ Point2 $ convertToGLFrame width height (xMouse, yMouse)
        modifyIORef ref ((p, white) :)

main :: IO ()
main = do
    W.setErrorCallback (Just errorCallBack)
    successfulInit <- W.init

    bool successfulInit exitFailure $ do
        W.windowHint $ W.WindowHint'ContextVersionMajor 2
        W.windowHint $ W.WindowHint'ContextVersionMinor 1

        W.windowHint $ W.WindowHint'Resizable False

        mw <- W.createWindow width height "OpenGL" Nothing Nothing
        maybe' mw (W.terminate >> exitFailure) $ \window -> do
            W.makeContextCurrent mw
            W.setKeyCallback window (Just keyCallback)
            let renderList = []
            renderRef <- newIORef renderList
            W.setMouseButtonCallback window (Just $ mouseButtonCallback renderRef)

            clearColor $= Color4 0 0 0 0
            pointSize $= 3.0
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

