{-# LANGUAGE TemplateHaskell, OverloadedLists #-}

module Data.Scene where

import           Numeric.LinearAlgebra (Vector, scale)
import qualified Graphics.GL as GLRaw
import           Control.Lens
import           Control.Arrow
import qualified Data.Vector as V


import           Data.GameObject ( GameObject(..)
                                 , drawObject
                                 , location )
import           Utils (lookAt, step, norm)

data Objects = Objects {
  _player :: GameObject,
  _npc    :: V.Vector GameObject,
  _area   :: V.Vector GameObject }

data Scene = Scene {
  _campos     :: Vector GLRaw.GLfloat,
  _objects    :: Objects,
  _projection :: Vector GLRaw.GLfloat }

makeLenses ''Objects

makeLenses ''Scene

traverseObjects :: (GameObject -> GameObject) -> Scene -> Scene
traverseObjects =
  over (objects.player) &&& over (objects.npc.traverse) >>> uncurry (.)


class Drawable d where
  draw :: d -> IO ()

instance Drawable Scene where
  draw (Scene pos obj proj) =
    let camMat     = lookAt pos $ obj^.player.location
        objectDraw = drawObject camMat proj
    in
      objectDraw (obj^.player)
      >> V.mapM_ objectDraw (obj^.npc)
      >> V.mapM_ objectDraw (obj^.area)

-- CAMERA

moveCameraV = over campos . (+)

moveCamera :: Float -> Float -> Float -> Scene -> Scene
moveCamera x y z = moveCameraV [x,y,z]

moveDepth :: Float -> Scene -> Scene
moveDepth = moveCamera 0 0

moveForwardCamera :: Scene -> Scene
moveForwardCamera = moveDepth step

moveBackCamera :: Scene -> Scene
moveBackCamera = moveDepth (-step)

adjustCamera :: Scene -> Scene
adjustCamera scene =
  let playPos = scene^.objects.player.location
      pointer = playPos - scene^.campos
      dist    = norm pointer
      maxDist = 5
      tooFar  = dist > maxDist
      movement= scale ((dist-maxDist)/dist) pointer
  in if tooFar then over campos (+movement) scene
     else scene


moveHor :: Float -> Scene -> Scene
moveHor s = moveCamera s 0 0

moveLeftCamera :: Scene -> Scene
moveLeftCamera = moveHor step

moveRightCamera :: Scene -> Scene
moveRightCamera = moveHor (-step)
