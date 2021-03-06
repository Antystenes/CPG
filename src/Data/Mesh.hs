{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedLists, BangPatterns, Strict #-}
module Data.Mesh where


import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Control.Lens      hiding (indices)
import qualified Data.Vector.Storable as VS
import           Number.Quaternion (T,real,imag)
import           Numeric.LinearAlgebra (Vector)
import qualified SDL
import qualified SDL.Image as SDLImage
import           Foreign (sizeOf)
import           Foreign.Ptr (intPtrToPtr)
import           Data.Shaders
import           Utils.Quaternions (qZero)
import           Control.Arrow
import           Control.Monad (unless)
import Control.DeepSeq
import Graphics.Rendering.OpenGL.GLU.Errors


instance NFData a => NFData (T a) where
  rnf q = let !_ = real q
              (!_,!_,!_) = imag q
          in ()

instance NFData Mesh where
  rnf m = let !_ = _quaternion m
          in ()

data Mesh = Mesh { _vertices     :: [Vector Float],
                   _indices      :: Vector GL.BaseInstance,
                   _vao          :: GL.VertexArrayObject,
                   _vbo          :: GL.BufferObject,
                   _ebo          :: GL.BufferObject,
                   _shaders      :: Shaders,
                   _quaternion   :: T Float,
                   _texture      :: Maybe GL.TextureObject,
                   _normalMap    :: Maybe GL.TextureObject }

makeLenses ''Mesh

-- loadAndBindImage :: FilePath -> IO ()
loadAndBindImage texType fn = do
  sdlSurf <- SDLImage.load fn
  SDL.V2 w h <- fmap fromIntegral <$> SDL.surfaceDimensions sdlSurf
  SDL.lockSurface sdlSurf
  texturePtr <- SDL.surfacePixels sdlSurf
  let textureSize = GL.TextureSize2D w h
      textureData = GL.PixelData GL.RGB GL.UnsignedByte texturePtr
  GL.texImage2D texType GL.NoProxy 0 GL.RGB' textureSize 0 textureData
  SDL.unlockSurface sdlSurf

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture fn = do
  texName <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just texName
  loadAndBindImage GL.Texture2D fn
  GL.textureFilter GL.Texture2D $=
    ((GL.Linear', Nothing),GL.Linear')
  return texName

createMesh (vdata, indices, verts) shad mtex norm = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao
    ebo <- createAndBindArrayBuffer GL.ElementArrayBuffer $ indices
    vbo <- createAndBindArrayBuffer GL.ArrayBuffer $ vdata

    setAttrib 17 3 "vertexPosition_modelspace" 0  -- vertex location
    setAttrib 17 3 "vertexNormal" 3  -- vertex normal
    setAttrib 17 2 "vertexUV" 6  -- vertex UV
    setAttrib 17 3 "vertex_col" 8  -- vertex color
    setAttrib 17 3 "vertex_tangent" 11 -- vertex tangent
    setAttrib 17 3 "vertex_bitangent" 14 -- vertex bitangent

    GL.bindVertexArrayObject $= Nothing
    return $ Mesh verts indices vao vbo ebo shad qZero mtex norm
  where
    setAttrib size attrSize name offset = do
      let glFloatSize = sizeOf (undefined :: GL.GLfloat)
          glLocation  = fromIntegral $ size * glFloatSize
          glOffset    = intPtrToPtr $ fromIntegral(offset * glFloatSize)
      loc@(GL.AttribLocation n) <- GL.get $ GL.attribLocation (shad^.program) name
      unless (n > 1000000) $ do
        GL.vertexAttribPointer loc $=
          (GL.ToFloat,
           GL.VertexArrayDescriptor attrSize GL.Float glLocation glOffset)
        GL.vertexAttribArray loc $= GL.Enabled

sendData :: forall a. VS.Storable a => GL.BufferTarget -> VS.Vector a -> IO ()
sendData t d =
  VS.unsafeWith d $
    \vptr -> GL.bufferData t $=
               (fromIntegral $ VS.length d * sizeOf (undefined :: a),
                vptr,
                GL.StaticDraw)

createAndBindArrayBuffer
  :: forall a. VS.Storable a =>
  GL.BufferTarget
  -> VS.Vector a
  -> IO GL.BufferObject
createAndBindArrayBuffer arrayType arrayData = do
    bufferObject <- GL.genObjectName
    GL.bindBuffer arrayType $= Just bufferObject
    sendData arrayType arrayData
    return bufferObject


vertexBufferData :: (VS.Vector Float, VS.Vector GL.BaseInstance, [VS.Vector Float])
vertexBufferData =
  let v = --position        normal         UV        color          tangents
          [-2.0, 0.0, -2.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0,
            2.0, 0.0, -2.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0,
           -2.0, 0.0,  2.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0,
            2.0, 0.0,  2.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0]
      v3 = [[-2.0, 0.0, -2.0],
            [ 2.0, 0.0, -2.0],
            [-2.0, 0.0,  2.0],
            [ 2.0, 0.0,  2.0]]
      i = [0,1,2,1,3,2]
  in (v,i,v3)

flatQuad shad tex norm =
  createMesh vertexBufferData shad (Just tex) (Just norm)

update :: (Float -> Float -> Bool) -> Int -> Float -> Vector Float -> Vector Float
update pred ix f v = if pred f (v VS.! ix) then VS.update_ v [ix] [f] else v

updatePair pred ix f = update pred ix f *** update ((not .) . pred) ix f

-- getAABB :: Mesh -> (Vector Float, Vector Float)
-- getAABB = VS.ifoldr inserter ([0,0,0],[0,0,0]) . view vertices
--   where inserter ix = case mod ix 17 of
--           1 -> updatePair (<) 0
--           2 -> updatePair (<) 1
--           3 -> updatePair (<) 2
--           _ -> flip const
