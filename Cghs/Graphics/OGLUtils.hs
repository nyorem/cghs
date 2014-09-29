-- | OpenGL utilities.
module Cghs.Graphics.OGLUtils
where

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL

-- | Converts a screen coordinates to OpenGL coordinates.
convertToGLFrame :: Int -> Int -> (Double, Double) -> (GLfloat, GLfloat)
convertToGLFrame w h (xMouse, yMouse) = (realToFrac x', realToFrac y')
    where x' =  2 * (xMouse / fromIntegral w) - 1
          y' = -2 * (yMouse / fromIntegral h) + 1

-- | Gets the cursor position converted in OpenGL coordinates.
getCursorPosConverted :: W.Window -> Int -> Int -> IO (GLfloat, GLfloat)
getCursorPosConverted window w h = do
    (xMouse, yMouse) <- W.getCursorPos window
    return $ convertToGLFrame w h (xMouse, yMouse)

-- | Some common OpenGL parameters.
initGLParams :: IO ()
initGLParams = do
    clearColor $= Color4 0 0 0 0
    pointSize $= 3.0
    polygonMode $= (Line, Line)

