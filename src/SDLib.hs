{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SDLib where

import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Control.Monad
import qualified Numeric.LinearAlgebra as L
import           Numeric.LinearAlgebra ((><))
import           Numeric.LinearAlgebra.Data (flatten)
import qualified Numeric.LinearAlgebra.Data as LD
import           Control.Lens      hiding (indices)
import           Debug.Trace
import           Control.Arrow

import           Utils (step)

import           OBJReader
import           Data.Shaders (loadShaders)

import           Data.Mesh    ( loadTexture
                              , createMesh
                              , flatQuad
                              -- lenses
                              , quaternion)
import           Data.PhysicsData ( acc, speed
                                  , angularS)
import           Data.GameObject
import           Utils.Quaternions
import           Data.Scene
import           Physics.Mechanics
import           Tasks.Swarm
import           EventHandler


frameTime = round $ 1000/60

mainLoop :: Float -> SDL.Window -> Scene -> IO ()
mainLoop time window scene = do
  begTime <- SDL.ticks
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  draw scene
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  kState <- SDL.getKeyboardState
  let qpressed   = any handleQuit events
      newTime    = time + (0.1*step)
      newTime2   = if newTime > 1 then 0 else newTime
      handleKeys = foldr ((.).keyHandler) id $ filter kState keys
      loc        = scene^.objects.player.location
      floc       = scene^.objects.npc
      updateNPCS = traction . addSeparationForce floc . addGravityForce loc
      newScene   = -- set objects.player.location) (countPosition newTime2)
                 adjustCamera
                 . over (objects.npc.traverse) updateNPCS
                 . traverseObjects (applyForce.traction)
                 $ handleKeys scene
  print $ newScene^.objects.player.physics
  print $ quatToMat3 $ newScene^.objects.player.mesh.quaternion
  print $ newScene^.objects.player.mesh.quaternion
  endTime <- SDL.ticks

  when ((endTime - begTime) < frameTime)
    $ SDL.delay (frameTime - (endTime - begTime))
  print $ endTime - begTime
  unless qpressed $ mainLoop newTime2 window newScene

sdlMain :: IO ()
sdlMain = do
  SDL.initializeAll
  window <- SDL.createWindow "yolo" SDL.defaultWindow {
      SDL.windowOpenGL = Just SDL.defaultOpenGL
    , SDL.windowInitialSize = SDL.V2 1366 768 }
  context <- SDL.glCreateContext window
  GL.depthFunc $= Just GL.Less
  let uniforms = ["camera","proj","pos","rot","texSampler","normSampler"] :: [String]
  shad <- loadShaders "shaders/vert.glsl" "shaders/frag.glsl" uniforms
  shadNoTex <- loadShaders "shaders/vert.glsl" "shaders/frag_no_text.glsl" uniforms
  ultraMesh <- readOBJ "media/untitled.obj"
  ultraVeh  <- readOBJ "media/vehicle.obj"
  let miniVeh = first (LD.cmap (*0.5)) ultraVeh
  meh       <- createMesh ultraVeh shadNoTex Nothing Nothing
  veh       <- createMesh miniVeh shadNoTex Nothing Nothing
  -- crvMesh   <- createMesh curveVertices shadNoTex Nothing
  tex       <- loadTexture "media/grass.jpg"
  normalTex <- loadTexture "media/normal2.png"

  qd        <- flatQuad shad tex normalTex
  let quads = map (\(x,z) -> GameObject qd [x, -2, z] Nothing []) [(x,z) | x <- [60,50..0], z <- [-50,-40..0]]
      n = 0.1
      f = 100
      t = 1 / tan ((pi/3) / 2)
      as= 1366/768
      proj = (4><4) [ t / as, 0,           0, 0
                    , 0,      t,           0, 0
                    , 0,      0, (f+n)/(n-f),(2*f*n)/(n-f)
                    , 0,      0,          -1, 0]
      camPos = [ 0, 10,20]
      vehOb pos = initializePhysics $ GameObject veh pos Nothing []
      vehs = [vehOb [x,y,0] | x<- [1..5], y <- [1..5]]
      mehOb = initializePhysics $
        GameObject meh [0,0.1,-3] Nothing []
      -- curve = GameObject crvMesh [0,0,0] Nothing []
      scene = Scene camPos (Objects mehOb [] quads) (flatten . L.tr $ proj)
  mainLoop 0 window scene
  GL.finish
  SDL.glDeleteContext context
  SDL.quit
