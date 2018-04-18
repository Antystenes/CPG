{-# LANGUAGE TemplateHaskell #-}
module Data.Mesh where


import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Control.Lens      hiding (indices)
import qualified Data.Vector.Storable as VS
import           Number.Quaternion (T)
import           Numeric.LinearAlgebra (Matrix, Vector)
import qualified SDL
import qualified SDL.Image as SDLImage

import           Data.Shaders

data Mesh = Mesh { _vertices     :: Vector Float,
                   _indices      :: Vector GL.BaseInstance,
                   _vao          :: GL.VertexArrayObject,
                   _vbo          :: GL.BufferObject,
                   _ebo          :: GL.BufferObject,
                   _shaders      :: Shaders,
                   _rotation     :: Maybe (Matrix Float),
                   _quaternion   :: T Float,
                   _texture      :: Maybe GL.TextureObject,
                   _normalMap    :: Maybe GL.TextureObject }

makeLenses ''Mesh


loadTexture :: FilePath -> IO GL.TextureObject
loadTexture fn = do
  sdlSurf <- SDLImage.load fn
  texName <- GL.genObjectName
  SDL.V2 w h <- fmap fromIntegral <$> SDL.surfaceDimensions sdlSurf
  SDL.lockSurface sdlSurf
  texturePtr <- SDL.surfacePixels sdlSurf
  let textureSize = GL.TextureSize2D w h
      textureData = GL.PixelData GL.RGB GL.UnsignedByte texturePtr
  GL.textureBinding GL.Texture2D $= Just texName
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB' textureSize 0 textureData
  GL.textureFilter GL.Texture2D $=
    ((GL.Linear', Nothing),GL.Linear')
  SDL.unlockSurface sdlSurf
  return texName
