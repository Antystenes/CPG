{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
module SDLib where

import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Control.Monad
import qualified Numeric.LinearAlgebra as L
import           Numeric.LinearAlgebra.Data (flatten)
import qualified Numeric.LinearAlgebra.Data as LD
import           Control.Lens      hiding (indices)
import           Debug.Trace
import           Control.Arrow
import qualified Data.Vector as V

import           Utils (step, projectionMatrix,normalize,clampSpeed)

import           OBJReader
import           Data.Shaders (loadShaders)

import           Data.Mesh    ( loadTexture
                              , createMesh
                              , flatQuad)
import           Data.GameObject
import           Data.Scene
import           Data.PhysicsData (acc)
import           Physics.Mechanics
import           Tasks.Swarm
import           EventHandler

frameTime = round $ 1000/60

mainLoop :: Float -> SDL.Window -> Scene -> IO ()
mainLoop time window scene = do
  begTime <- SDL.ticks
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  draw scene
  putStrLn =<< ("Draw Time: "++) . show . subtract begTime <$> SDL.ticks
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  kState <- SDL.getKeyboardState
  let qpressed   = any handleQuit events
      newTime    = time + (0.1*step)
      newTime2   = if newTime > 1 then 0 else newTime
      handleKeys = foldr ((.).keyHandler) id $ filter kState keys
      loc        = scene^.objects.player.location
      updateNPCS = -- over (physics._Just.acc) clampSpeed .
                 applyLinear .
                 -- friction .
                 over (physics._Just.acc) (L.scale 3 . normalize) .
                 addGravityForce loc
      newScene   = -- set objects.player.location) (countPosition newTime2)
                 -- adjustCamera
                 over (objects.npc.traverse) updateNPCS
                 . over (objects.player) (friction.applyForce)
                 . over (objects.npc) computeForces
                 $ handleKeys scene
  endTime <- SDL.ticks
  when ((endTime - begTime) < frameTime)
    $ SDL.delay (frameTime - (endTime - begTime))
  -- print . getAABB $ scene^.objects.player.mesh
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
  let quads = V.fromList $ map (\(x,z) -> GameObject qd [x, -20, z] Nothing []) [(x,z) | x <- [-20,-10..20], z <- [-20,-10..20]]
      camPos = [ 0, 10,20]
      vehOb pos = initializePhysics $ GameObject veh pos Nothing []
      dim = 4
      sep = 1
      vehs = V.fromList [vehOb [sep*x,sep*y,sep*z] | x<- [-dim..0], y <- [-dim..0], z <- [-dim..0]]
      mehOb = initializePhysics $
        GameObject meh [-5,0,-10] Nothing []
      -- curve = GameObject crvMesh [0,0,0] Nothing []
      scene = Scene camPos (Objects mehOb vehs quads) (flatten . L.tr $ projectionMatrix)
  mainLoop 0 window scene
  GL.finish
  SDL.glDeleteContext context
  SDL.quit
