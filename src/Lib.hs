{-#LANGUAGE OverloadedLists#-}
module Lib
    ( someFunc,
      loadShaders
    ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Control.Monad
import           Foreign (sizeOf, nullPtr)

data Mesh = Mesh { vertices :: VS.Vector Float,
                   vao      :: GL.VertexArrayObject}

windowHints :: IO ()
windowHints = do
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 5
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

vertexBufferData :: VS.Vector Float
vertexBufferData = [-1.0, -1.0, 0.0,
                      1.0, -1.0, 0.0,
                      0.0,  1.0, 0.0]

loadShaders :: String -> String -> IO GL.Program
loadShaders vertNm fragNm = do
  vertexShader   <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader
  vertSrc        <- BS.readFile vertNm
  fragSrc        <- BS.readFile fragNm
  GL.shaderSourceBS vertexShader $= vertSrc
  GL.shaderSourceBS fragmentShader $= fragSrc
  GL.compileShader vertexShader
  GL.shaderInfoLog vertexShader >>= print . ("Vert Log:\n"++)
  GL.compileShader fragmentShader
  GL.shaderInfoLog fragmentShader >>= print . ("Frag Log:\n"++)
  prog <- GL.createProgram
  GL.attachShader prog vertexShader
  GL.attachShader prog fragmentShader
  GL.linkProgram prog
  GL.detachShader prog vertexShader
  GL.detachShader prog fragmentShader
  return prog

mainLoop :: GLFW.Window -> GL.BufferObject -> GL.Program -> IO ()
mainLoop w vbo shaders = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.currentProgram $= Just shaders
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
  GL.drawArrays GL.Triangles 0 3
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
  GLFW.swapBuffers w
  GLFW.pollEvents
  GLFW.getKey w GLFW.Key'Escape >>= \x -> unless (x == GLFW.KeyState'Pressed) $ mainLoop w vbo shaders

someFunc :: IO ()
someFunc = GLFW.init >>=
  \x -> if x
    then do
       windowHints
       mwin <- GLFW.createWindow 1280 720 "Yolo" Nothing Nothing
       case mwin of
         Nothing -> do
           print "nie udało się otworzyć okna"
           return ()
         Just win -> do
           GLFW.makeContextCurrent mwin
           GLFW.setStickyKeysInputMode win GLFW.StickyKeysInputMode'Enabled
           triVao <- GL.genObjectName
           GL.bindVertexArrayObject $= Just triVao
           let triangle = Mesh vertexBufferData triVao
           vbo <- GL.genObjectName
           GL.bindBuffer GL.ArrayBuffer $= Just vbo
           VS.unsafeWith vertexBufferData $ \vptr -> GL.bufferData GL.ArrayBuffer $=
             (fromIntegral $ VS.length vertexBufferData * sizeOf (0 :: Float), vptr, GL.StaticDraw)
           shaders <- loadShaders "shaders/vert.glsl" "shaders/frag.glsl"
           mainLoop win vbo shaders
           GLFW.destroyWindow win
           GLFW.terminate
     else do
       print "Nie udało się zainicjalizować glfw"
       return ()
