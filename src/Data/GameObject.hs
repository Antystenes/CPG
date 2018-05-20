{-# LANGUAGE TemplateHaskell #-}

module Data.GameObject where

import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra (Vector, flatten)
import           Control.Lens      hiding (indices)
import           Number.Quaternion ((+::))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.GL as GLRaw
import qualified Data.HashMap               as HM
import           Foreign.Ptr (intPtrToPtr, IntPtr(..))

import           Data.Mesh
import           Data.PhysicsData
import           Utils.Quaternions
import           Data.Shaders

import           Utils (posToMatV,glTexture)

data GameObject = GameObject {
  _mesh     :: Mesh,
  _location :: Vector Float,
  _physics  :: Maybe PhysicsData,
  _children :: [GameObject] }

instance Show GameObject where
  show (GameObject _ l p _) = show l ++ "\n" ++ show p ++ "\n"

makeLenses ''GameObject

initializePhysics :: GameObject -> GameObject
initializePhysics obj =
  let quat = obj^.mesh.quaternion
  in set physics (Just $ startPhysics quat) obj

turnQuatZ :: Float -> GameObject -> GameObject
turnQuatZ = over (mesh.quaternion) . quatConcat . rotQuatZ
  where
    rotQuatZ t = cos (t/2) +:: (0,sin $ t / 2,0)

drawObject cam proj obj = do
  GL.currentProgram $= Just (obj^.mesh.shaders.program)
  GL.bindVertexArrayObject $= Just (obj^.mesh.vao)
  passMatrix "camera" $ flatten cam
  passMatrix "proj" proj
  passMatrix "rot" . quatToMatV $ obj^.mesh.quaternion
  passMatrix "pos" . posToMatV $ obj^.location

  passTexture "texSampler" 1 $ obj^.mesh.texture
  passTexture "normSampler" 2 $ obj^.mesh.normalMap

  GL.drawElements GL.Triangles indLen GL.UnsignedInt (intPtrToPtr $ IntPtr 0)

  GL.bindVertexArrayObject $= Nothing
  where
    indLen    = fromIntegral . VS.length $ obj^.mesh.indices
    uniformName name =
      HM.findWithDefault (-1) name $ obj^.mesh.shaders.uniforms
    passMatrix name mat =
      VS.unsafeWith mat $
         GLRaw.glUniformMatrix4fv (uniformName name) 1 0
    passTexture name pos mtex = do
      GLRaw.glActiveTexture $ glTexture pos
      GL.textureBinding GL.Texture2D $= mtex
      GL.uniform (GL.UniformLocation $ uniformName name) $= GL.TextureUnit pos
