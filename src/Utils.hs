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
import qualified Numeric.LinearAlgebra as L
import           Data.Vector.Storable ((!))
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Data.Vector.Storable as VS

step :: Float
step = 0.02

norm :: L.Vector Float -> Float
norm = realToFrac . L.norm_2


posToMat vec = let pref = [1,0,0,0
                           ,0,1,0,0
                           ,0,0,1,0]
                in LD.reshape 4 $ pref VS.++ VS.snoc vec 1

glTexture :: GL.GLuint -> GLRaw.GLenum
glTexture = (GLRaw.GL_TEXTURE0+) . fromIntegral


lookAt' :: L.Vector Float -> L.Vector Float -> L.Vector Float -> L.Matrix Float
lookAt' up eye at =
  let zaxis = normalize $ eye - at
      xaxis = normalize $ L.cross zaxis up
      yaxis = L.cross xaxis zaxis
      crd x = negate $ L.dot eye x
  in (4L.><4) [ xaxis!0,   yaxis!0,   zaxis!0,   0
              , xaxis!1,   yaxis!1,   zaxis!1,   0
              , xaxis!2,   yaxis!2,   zaxis!2,   0
              , crd xaxis, crd yaxis, crd zaxis, 1]


lookAt :: L.Vector Float -> L.Vector Float -> L.Matrix Float
lookAt = lookAt' [0,1,0]


normalize :: L.Vector Float -> L.Vector Float
normalize v =
  let n = (1/) . norm $ v
  in L.scale n v

mat3ToMat4 :: L.Matrix Float -> L.Matrix Float
mat3ToMat4 = LD.fromLists . foldr (\a b -> (a ++ [0]) : b) [[0,0,0,1]] . LD.toLists
