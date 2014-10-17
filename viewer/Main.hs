module Main where

import Control.Lens
import Control.Monad ( when )
import Data.IORef
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL

import Cghs.Graphics.Event
import Cghs.Graphics.RenderableItem
import Cghs.Graphics.Types
import Cghs.Graphics.Utils

import Cghs.Utils

errorCallBack :: W.ErrorCallback
errorCallBack _ desc = hPutStrLn stderr desc

keyCallback :: IORef ViewerState -> W.KeyCallback
keyCallback ref window key _ action mods =
    when (action == W.KeyState'Pressed) $ do
        case lookup key keyEventFunctions of
            Nothing -> return ()
            Just f -> f ref mods window

mouseButtonCallback :: IORef ViewerState -> W.MouseButtonCallback
mouseButtonCallback ref window button state _ = do
    when (state == W.MouseButtonState'Pressed) $ do
        case lookup button mouseEventFunctions of
            Nothing -> return ()
            Just f -> f ref window

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
        changeTitle viewerState window
        renderItemList $ viewerState ^. renderList

        W.pollEvents
        W.swapBuffers window

        mainLoop ref window

-- | Change the title of the window according to the current
-- selection mode.
changeTitle :: ViewerState -> W.Window -> IO ()
changeTitle state w = do
    let mode = state ^. selectionMode
    W.setWindowTitle w $ "cghs - " ++ show mode

