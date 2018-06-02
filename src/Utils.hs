{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}

module Utils where

import qualified Graphics.GL as GLRaw
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Numeric.LinearAlgebra ((><), Vector, Matrix, scale, cross, dot)
import qualified Numeric.LinearAlgebra as L
import           Data.Vector.Storable ((!))
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Data.Vector.Storable as VS

step :: Float
step = 0.02

norm :: Vector Float -> Float
norm = realToFrac . L.norm_2

posToMatV :: Vector Float -> Vector Float
posToMatV v =
            [1,0,0,0
             ,0,1,0,0
             ,0,0,1,0
             ,VS.unsafeIndex v 0,VS.unsafeIndex v 1, VS.unsafeIndex v 2,1]

posToMat = LD.reshape 4 . posToMatV

glTexture :: GL.GLuint -> GLRaw.GLenum
glTexture = (GLRaw.GL_TEXTURE0+) . fromIntegral

lookAt' :: Vector Float -> Vector Float -> Vector Float -> Matrix Float
lookAt' up eye at =
  let zaxis = normalize $ eye - at
      xaxis = normalize $ cross zaxis up
      yaxis = cross xaxis zaxis
      crd x = negate $ dot eye x
  in (4><4) [ xaxis!0,   yaxis!0,   zaxis!0,   0
            , xaxis!1,   yaxis!1,   zaxis!1,   0
            , xaxis!2,   yaxis!2,   zaxis!2,   0
            , crd xaxis, crd yaxis, crd zaxis, 1]

pointAt :: Vector Float -> Vector Float
pointAt sp =
  let up =  [0,1,0]
      eye = [0,0,0]
      at  = sp
      zaxis = normalize $ eye - at
      xaxis = normalize $ cross zaxis up
      yaxis = cross xaxis zaxis
      crd x = negate $ dot eye x
  in [ xaxis!0,   yaxis!0,   zaxis!0,   0
     , xaxis!1,   yaxis!1,   zaxis!1,   0
     , xaxis!2,   yaxis!2,   zaxis!2,   0
     , crd xaxis, crd yaxis, crd zaxis, 1]


lookAt :: Vector Float -> Vector Float -> Matrix Float
lookAt = lookAt' [0,1,0]


normalize :: Vector Float -> Vector Float
normalize v =
  let n = (1/) . norm $ v
  in scale n v

remTrans = mat3ToMat4 . mat4ToMat3

mat4ToMat3 :: Matrix Float -> Matrix Float
mat4ToMat3 = LD.takeRows 3 . LD.takeColumns 3

mat3ToMat4 :: Matrix Float -> Matrix Float
mat3ToMat4 = helper . LD.flatten
  -- LD.fromLists . foldr (\a -> ((a ++ [0]):)) [[0,0,0,1]] . LD.toLists
  where
    helper v = (4><4) [ VS.unsafeIndex v 0, VS.unsafeIndex v 1, VS.unsafeIndex v 2, 0
                      , VS.unsafeIndex v 3, VS.unsafeIndex v 4, VS.unsafeIndex v 5, 0
                      , VS.unsafeIndex v 6, VS.unsafeIndex v 7, VS.unsafeIndex v 8, 0
                      ,                  0,                  0,                  0, 1]

projectionMatrix :: Matrix Float
projectionMatrix =
  (4><4) [ t / as, 0,           0, 0
         , 0,      t,           0, 0
         , 0,      0, (f+n)/(n-f),(2*f*n)/(n-f)
         , 0,      0,          -1, 0]
  where
    n = 0.1
    f = 200
    t = 1 / tan ((pi/3) / 2)
    as= 1366/768

maxSpeed = 10

clampSpeed :: Vector Float -> Vector Float
clampSpeed v =
  if norm v >= maxSpeed
  then LD.cmap (*maxSpeed) . normalize $ v
  else v
