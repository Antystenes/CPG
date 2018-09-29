{-# LANGUAGE TemplateHaskell, OverloadedLists #-}
{-# LANGUAGE Strict #-}

module Data.Scene where

import           Numeric.LinearAlgebra (Vector, scale)
import qualified Graphics.GL as GLRaw
import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Control.Lens
import           Control.Arrow
import qualified Data.Vector as V

import           Data.GameObject ( GameObject(..)
                                 , drawObject
                                 , location
                                 , drawObject2)
import           Utils (lookAt, step, norm, remTrans)
import           Data.SkyBox
import           Tasks.Swarm
import           Debug.Trace
-- import           Data.Traversable (traverse)

data Objects = Objects {
  _player :: GameObject,
  _npc    :: V.Vector GameObject,
  _area   :: V.Vector GameObject }

data Scene = Scene {
  _campos     :: Vector GLRaw.GLfloat,
  _objects    :: Objects,
  _projection :: Vector GLRaw.GLfloat,
  _skyBox :: Maybe SkyBox }

makeLenses ''Objects

makeLenses ''Scene

resolveObjectCollisions obs =
  let v = resolveContacts .
          V.cons (obs^.player) $
          (obs^.npc) -- V.++ (obs^.area)
      newPlayer = V.head v
      newNpcs =
        -- V.take (V.length (obs^.npc)) .
        V.drop 1 $ v
  in set player newPlayer . set npc newNpcs $ obs

traverseObjects :: (GameObject -> GameObject) -> Scene -> Scene
traverseObjects =
  over (objects.player) &&& over (objects.npc.traverse) >>> uncurry (.)


class Drawable d where
  draw :: SDL.Window -> d -> IO ()

instance Drawable Scene where
  draw window (Scene pos obj proj msb) =
    let camMat     = lookAt pos $ obj^.player.location
        objectDraw = drawObject (msb^?_Just.skyTex) camMat proj
        objectDraw2 = drawObject2 camMat proj
        drawSB      = drawSkyBox (remTrans camMat) proj
    in
      GL.clear [GL.ColorBuffer, GL.DepthBuffer]
      >> traverse drawSB msb
      >> objectDraw (obj^.player)
      >> V.mapM_ objectDraw (obj^.npc)
      >> V.mapM_ objectDraw (obj^.area)
      >> SDL.glSwapWindow window

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
      maxDist = 10
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
