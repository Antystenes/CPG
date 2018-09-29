{-# LANGUAGE OverloadedStrings
           , OverloadedLists
           , TypeApplications
           , ExplicitForAll
           , ScopedTypeVariables
           , AllowAmbiguousTypes #-}
module Core where
import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))
import Control.Exception
import Foreign
import Foreign.C.String (newCAStringLen)
import Data.Vector.Storable

import Prelude hiding (length)

import Data.Mesh (createAndBindArrayBuffer)
import Data.Shaders

setup :: IO ()
setup = do
  SDL.swapInterval $= SDL.SynchronizedUpdates
  GL.depthFunc $= Just GL.Less

dataSize :: forall a. Storable a => Int
dataSize = sizeOf yolo
  where yolo = undefined :: a


vertices :: Vector GL.GLfloat
vertices = [
        -0.5, -0.5, 0.0, -- first vertex
        0.5, -0.5, 0.0, -- second vertex
        0.0,  0.5, 0.0 -- third vertex
        ]

vecSize :: forall a. (Storable a) => Vector a -> Int
vecSize v = fromIntegral $ dataSize @a * length v


mainFunction :: IO ()
mainFunction = do
  SDL.initializeAll
  window <- SDL.createWindow "yolo" SDL.defaultWindow {
      SDL.windowOpenGL = Just SDL.defaultOpenGL
    , SDL.windowInitialSize = SDL.V2 1366 768 }
  context <- SDL.glCreateContext window
  setup
  vbo <- createAndBindArrayBuffer GL.ArrayBuffer vertices
  GL.finish
  -- SDL.glDeleteContext context
  SDL.quit
