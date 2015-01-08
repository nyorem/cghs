module Main where

import Control.Lens
import Control.Monad ( unless, when )
import Data.IORef
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL

import Cghs.Graphics.Event
import Cghs.Graphics.RenderableItem
import Cghs.Graphics.Types
import Cghs.Graphics.Utils

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

initialize :: String -> IORef ViewerState -> IO W.Window
initialize title stateRef = do
    W.setErrorCallback (Just errorCallBack)
    successfulInit <- W.init

    if not successfulInit then exitFailure else do
        W.windowHint $ W.WindowHint'ContextVersionMajor 2
        W.windowHint $ W.WindowHint'ContextVersionMinor 1
        W.windowHint $ W.WindowHint'Resizable False

        mw <- W.createWindow width height title Nothing Nothing
        case mw of
            Nothing -> W.terminate >> exitFailure
            Just window -> do
                W.makeContextCurrent mw
                W.setKeyCallback window (Just $ keyCallback stateRef)
                W.setMouseButtonCallback window (Just $ mouseButtonCallback stateRef)

                initGLParams
                return window

main :: IO ()
main = do
    stateRef <- newIORef initialViewerState
    w <- initialize "cghs" stateRef
    mainLoop stateRef w
    cleanup w

cleanup :: W.Window -> IO ()
cleanup w = do
    W.destroyWindow w
    W.terminate
    exitSuccess

mainLoop :: IORef ViewerState -> W.Window -> IO ()
mainLoop ref window = do
        close <- W.windowShouldClose window
        unless close $ do
            clear [ColorBuffer]
            viewerState <- readIORef ref
            changeTitle viewerState window

            renderItemList $ viewerState ^. renderList

            W.swapBuffers window
            W.pollEvents
            mainLoop ref window

changeTitle :: ViewerState -> W.Window -> IO ()
changeTitle state w = do
    let mode = state ^. selectionMode
    W.setWindowTitle w $ "cghs - " ++ show mode

