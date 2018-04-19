{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Utils where

import           Language.Haskell.TH.Syntax
import           Data.Proxy
import qualified Graphics.GL as GLRaw
import           GHC.TypeLits
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


posToMat vec = let pref = [1,0,0,0
                           ,0,1,0,0
                           ,0,0,1,0]
                in LD.reshape 4 $ pref VS.++ VS.snoc vec 1

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


lookAt :: Vector Float -> Vector Float -> Matrix Float
lookAt = lookAt' [0,1,0]


normalize :: Vector Float -> Vector Float
normalize v =
  let n = (1/) . norm $ v
  in scale n v

mat3ToMat4 :: Matrix Float -> Matrix Float
mat3ToMat4 = LD.fromLists . foldr (\a b -> (a ++ [0]) : b) [[0,0,0,1]] . LD.toLists

projectionMatrix :: Matrix Float
projectionMatrix =
  (4><4) [ t / as, 0,           0, 0
         , 0,      t,           0, 0
         , 0,      0, (f+n)/(n-f),(2*f*n)/(n-f)
         , 0,      0,          -1, 0]
  where
    n = 0.1
    f = 100
    t = 1 / tan ((pi/3) / 2)
    as= 1366/768
