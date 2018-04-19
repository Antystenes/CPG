{-# LANGUAGE OverloadedLists #-}
module Tasks.Curve where
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra (Matrix, Vector, flatten,(><))
import           Data.Monoid ((<>))

hermiteMatrix :: Matrix Float
hermiteMatrix = (4><4)
  [ 2,-2, 1, 1
  ,-3, 3,-2,-1
  , 0, 0, 1, 0
  , 1, 0, 0, 0 ]

curvePoints :: Matrix Float
curvePoints = (4><3)
  [ 20, 0, -10
  , 20, 0, -10
  , 100, 100, 0
  , 100, -100, 0]

countPosition :: Float -> Vector Float
countPosition t = flatten $ (1><4) [t^3,t^2,t^1,1] <> hermMat
  where
    hermMat = hermiteMatrix <> curvePoints

lowerPoints :: [Vector Float]
lowerPoints = map countPosition  [0,0.05..0.999]

curveVertices :: Vector Float
curveVertices = VS.concat . map (VS.++ vData) $ quad =<< [0..length lowerPoints - 2]
  where
    lowerTri i = [lVert $ i+1,lVert i,uVert i] :: [Vector Float]
    upperTri i = [lVert $ i+1, uVert i, uVert $ i+1] :: [Vector Float]
    quad i = lowerTri i ++ upperTri i
    lVert i = lowerPoints!!i
    uVert i = (lowerPoints!!i) + ([0,1,0] :: Vector Float)
    vData = [0,0,1,0,0,1,0,1,0,0,0,0,0,0] :: Vector Float
