{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Data.GameObject where

import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra (Vector, Matrix, flatten, (<>), tr,(#>),(><))
import           Control.Lens      hiding (indices)
import           Number.Quaternion ((+::))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.GL as GLRaw
import qualified Data.HashMap               as HM
import           Foreign.Ptr (intPtrToPtr, IntPtr(..))
import           Control.Arrow
import           Data.Maybe
import Data.Vector.Strategies
import GHC.Generics

import           Data.Mesh
import           Data.PhysicsData
import           Utils.Quaternions
import           Data.Shaders
import           Physics.Collision

import           Utils (posToMat,posToMatV,glTexture,pointAt)

data GameObject = GameObject {
  _mesh     :: Mesh,
  _location :: Vector Float,
  _physics  :: Maybe PhysicsData,
  _children :: [GameObject],
  _primitive:: Primitive
  }

instance Show GameObject where
  show (GameObject m l p _ pr) = show l ++ "\n" ++ show p ++ "\n" ++ show (m^.quaternion) ++ "\n" ++ show pr ++ "\n"

makeLenses ''GameObject

modelMat :: GameObject -> Matrix Float
modelMat =
  view location &&& view (mesh.quaternion)  >>>
  posToMat *** quatToMat >>>
  uncurry (<>) --- >>> tr

translatePoint :: GameObject -> Vector Float -> Vector Float
translatePoint obj =
  let rot = (#>) . tr . quatToMat3 $ obj^.mesh.quaternion
      tra = (obj^.location +)
  in tra . rot

prepareCPrim :: GameObject -> PrimitiveComputation
prepareCPrim =
  modelMat &&& view primitive >>>
  uncurry preparePrimitive

initializePhysics :: Float -> GameObject -> GameObject
initializePhysics mInv obj =
  let quat = obj^.mesh.quaternion
  in set physics (Just $ startPhysics mInv quat) obj

turnQuatZ :: Float -> GameObject -> GameObject
turnQuatZ = over (mesh.quaternion) . quatConcat . rotQuatZ
  where
    rotQuatZ t = cos (t/2) +:: (0,sin $ t / 2,0)

getInertiaInv :: GameObject -> Matrix Float
getInertiaInv =
  fromMaybe ((3><3) $ replicate 9 0) .
  preview (physics._Just.inertiaInv)

drawObject2 cam proj obj = do
  GL.currentProgram $= Just (obj^.mesh.shaders.program)
  GL.bindVertexArrayObject $= Just (obj^.mesh.vao)

  passMatrix "camera" $ flatten cam
  passMatrix "proj" proj
  mapM_ (passMatrix "rot" . pointAt . view speed) $ obj^.physics
  passMatrix "pos" . posToMatV $ obj^.location

  passTexture "texSampler" 0 $ obj^.mesh.texture
  passTexture "normSampler" 1 $ obj^.mesh.normalMap

  GL.drawElements GL.Triangles indLen GL.UnsignedInt (intPtrToPtr $ IntPtr 0)

  GL.bindVertexArrayObject $= Nothing
  where
    indLen    = fromIntegral . VS.length $ obj^.mesh.indices
    passMatrix = passMatrixToProg $ obj^.mesh.shaders
    passTexture = passTextureToProg GL.Texture2D $ obj^.mesh.shaders


passTextureToProg tp shaders name pos mtex = do
  GL.activeTexture $= GL.TextureUnit pos
  GL.textureBinding tp $= mtex
  GL.uniform (GL.UniformLocation $ uniformName shaders name) $= GL.TextureUnit pos
  return ()

passMatrixToProg shaders name mat =
  VS.unsafeWith mat $
     GLRaw.glUniformMatrix4fv (uniformName shaders name) 1 0

drawObject mskyTex cam proj obj = do
  GL.blend $= GL.Enabled
  -- GL.depthFunc $= Nothing

  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.currentProgram $= Just (obj^.mesh.shaders.program)
  GL.bindVertexArrayObject $= Just (obj^.mesh.vao)

  passMatrix "camera" $ flatten cam
  passMatrix "proj" proj
  passMatrix "rot" . quatToMatV $ obj^.mesh.quaternion
  passMatrix "pos" . posToMatV $ obj^.location

  passTexture "texSampler" 1 $ obj^.mesh.texture
  passTexture "normSampler" 2 $ obj^.mesh.normalMap
  passTexture "skytex" 1 mskyTex

  GL.drawElements GL.Triangles indLen GL.UnsignedInt (intPtrToPtr $ IntPtr 0)
  GL.blend $= GL.Disabled
  -- GL.depthFunc $= Just GL.Less


  GL.bindVertexArrayObject $= Nothing
  where
    indLen    = fromIntegral . VS.length $ obj^.mesh.indices
    passMatrix = passMatrixToProg $ obj^.mesh.shaders
    passTexture = passTextureToProg GL.Texture2D $ obj^.mesh.shaders
