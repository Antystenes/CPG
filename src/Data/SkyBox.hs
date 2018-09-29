{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module Data.SkyBox where

import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Numeric.LinearAlgebra (flatten,ident)
import           Control.Monad
import           Control.Lens
import qualified Data.Vector.Storable as VS
import           Foreign (sizeOf)
import           Foreign.Ptr (intPtrToPtr)

import Data.Mesh
import Data.GameObject
import Data.Shaders

data SkyBox = SkyBox { _skyTex :: GL.TextureObject,
                       _skyVao :: GL.VertexArrayObject,
                       _skyVbo :: GL.BufferObject,
                       _shad   :: Shaders }

makeLenses ''SkyBox

drawSkyBox cam proj b = do
  GL.depthMask $= GL.Disabled
  GL.currentProgram $= Just (b^.shad.program)
  GL.bindVertexArrayObject $= Just (b^.skyVao)

  passMatrix "camera" $ flatten cam
  passMatrix "proj" proj

  passTexture "skytex" 3 $ Just $ b^.skyTex
  GL.drawArrays GL.Triangles 0 36
  GL.depthMask $= GL.Enabled
  where
    passMatrix = passMatrixToProg $ b^.shad
    passTexture = passTextureToProg GL.TextureCubeMap $ b^.shad

loadSkyBox = do
  let uni = ["camera", "proj", "skytex"] :: [String]
  shad <- loadShaders "shaders/cubevert.glsl" "shaders/cubefrag.glsl" uni
  tex <- loadTextures fnames
  createSkyBox shad tex

createSkyBox shad tex = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  vbo <- createAndBindArrayBuffer GL.ArrayBuffer skyboxVertices
  let glFloatSize = sizeOf (undefined :: GL.GLfloat)
  loc <- GL.get $ GL.attribLocation (shad^.program) "aPos"
  GL.vertexAttribPointer loc $=
    (GL.ToFloat,
     GL.VertexArrayDescriptor 3 GL.Float 0 $ intPtrToPtr $ fromIntegral 0)
  GL.vertexAttribArray loc $= GL.Enabled
  GL.bindVertexArrayObject $= Nothing
  return $ SkyBox tex vao vbo shad


texTargets =
  [ GL.TextureCubeMapPositiveX
  , GL.TextureCubeMapNegativeX
  , GL.TextureCubeMapPositiveY
  , GL.TextureCubeMapNegativeY
  , GL.TextureCubeMapPositiveZ
  , GL.TextureCubeMapNegativeZ ]

fnames :: [String]
fnames = -- map (("media/SkyBox/"++).(++".jpg")) . replicate 6 $ "Starscape"
  map ("media/sbox2/cgp_1/textures/"++) [ "xpos.png", "xneg.png", "ypos.png", "yneg.png", "zpos.png", "zneg.png"]


  -- [ "right"
  -- , "left"
  -- , "up"
  -- , "bottom"
  -- , "front"
  -- , "back" ]

loadTextures fns = do
  texName <- GL.genObjectName
  GL.textureBinding GL.TextureCubeMap $= Just texName
  zipWithM_ loadAndBindImage texTargets fns
  GL.textureFilter GL.TextureCubeMap $=
    ((GL.Linear', Nothing),GL.Linear')
  mapM_ setWrapping dirs
  return texName
  where setWrapping dir =
          GL.textureWrapMode GL.TextureCubeMap dir $=
            (GL.Repeated, GL.ClampToBorder)
        dirs = [GL.S, GL.T, GL.R] :: [GL.TextureCoordName]

skyboxVertices :: VS.Vector Float
skyboxVertices = [
    -1.0,  1.0, -1.0,
    -1.0, -1.0, -1.0,
     1.0, -1.0, -1.0,
     1.0, -1.0, -1.0,
     1.0,  1.0, -1.0,
    -1.0,  1.0, -1.0,

    -1.0, -1.0,  1.0,
    -1.0, -1.0, -1.0,
    -1.0,  1.0, -1.0,
    -1.0,  1.0, -1.0,
    -1.0,  1.0,  1.0,
    -1.0, -1.0,  1.0,

     1.0, -1.0, -1.0,
     1.0, -1.0,  1.0,
     1.0,  1.0,  1.0,
     1.0,  1.0,  1.0,
     1.0,  1.0, -1.0,
     1.0, -1.0, -1.0,

    -1.0, -1.0,  1.0,
    -1.0,  1.0,  1.0,
     1.0,  1.0,  1.0,
     1.0,  1.0,  1.0,
     1.0, -1.0,  1.0,
    -1.0, -1.0,  1.0,

    -1.0,  1.0, -1.0,
     1.0,  1.0, -1.0,
     1.0,  1.0,  1.0,
     1.0,  1.0,  1.0,
    -1.0,  1.0,  1.0,
    -1.0,  1.0, -1.0,

    -1.0, -1.0, -1.0,
    -1.0, -1.0,  1.0,
     1.0, -1.0, -1.0,
     1.0, -1.0, -1.0,
    -1.0, -1.0,  1.0,
     1.0, -1.0,  1.0 ]
