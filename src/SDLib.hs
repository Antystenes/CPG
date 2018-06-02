{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.Vector.Strategies
import Control.Concurrent (forkOS, killThread)
import Control.Concurrent.MVar


import           Utils (step, projectionMatrix,normalize,clampSpeed)

import           OBJReader
import           Data.Shaders (loadShaders)

import           Data.Mesh    ( loadTexture
                              , createMesh
                              , flatQuad)
import           Data.GameObject
import           Data.Scene
import           Data.PhysicsData (acc,speed)
import           Data.SkyBox
import           Physics.Mechanics
import           Tasks.Swarm
import           EventHandler


frameTime = round $ 1000/60


-- drawThread ::

debugPrint (GL.DebugMessage _ GL.DebugTypeError _ _ err) = putStrLn err
debugPrint _ = return ()

drawThread mv window = forkOS $ do
  context <- SDL.glCreateContext window
  SDL.swapInterval $= SDL.SynchronizedUpdates
  GL.depthFunc $= Just GL.Less
  -- GL.debugMessageCallback $= Just debugPrint
  -- GL.debugOutput $= GL.Enabled
  let uniforms = ["camera","proj","pos","rot","texSampler","normSampler"] :: [String]
  shad <- loadShaders "shaders/vert.glsl" "shaders/frag.glsl" uniforms
  shadNoTex <- loadShaders "shaders/vert.glsl" "shaders/frag_no_text.glsl" uniforms
  ultraMesh <- readOBJ "media/untitled.obj"
  ultraVeh  <- readOBJ "media/vehicle.obj"
  let miniVeh =  ultraVeh
  meh       <- createMesh ultraVeh shadNoTex Nothing Nothing
  veh       <- createMesh miniVeh shadNoTex Nothing Nothing
  -- crvMesh   <- createMesh curveVertices shadNoTex Nothing
  tex       <- loadTexture "media/grass.jpg"
  normalTex <- loadTexture "media/normal2.png"
  sbox      <- loadSkyBox
  qd        <- flatQuad shad tex normalTex
  let quads = V.fromList $ map (\(x,z) -> GameObject qd [x, -2, z] Nothing []) [(x,z) | x <- [-60,-40..60], z <- [-60,-40..60]]
      camPos = [ 0, 10,20]
      vehOb pos = initializePhysics $ GameObject veh pos Nothing []
      dim = 0
      sep = 3
      vehs = V.fromList [vehOb [sep*x,sep*y+10,sep*z] | x<- [-dim..0], y <- [-dim..0], z <- [-dim..0]]
      mehOb = initializePhysics $
        GameObject meh [-5,0,-20] Nothing []
      -- curve = GameObject crvMesh [0,0,0] Nothing []
      scene = Scene camPos (Objects mehOb vehs quads) (flatten . L.tr $ projectionMatrix) (Just sbox)
  putMVar mv scene
  drawLoop
  where drawLoop = takeMVar mv >>= draw window >> drawLoop

mainLoop :: MVar Scene -> SDL.Window -> Scene -> IO ()
mainLoop mv window scene = do
  begTime <- SDL.ticks
  events <- SDL.pollEvents
  kState <- SDL.getKeyboardState
  let qpressed   = any handleQuit events
      -- newTime    = time + (0.1*step)
      -- newTime2   = if newTime > 1 then 0 else newTime
      handleKeys = foldr ((.).keyHandler) id $ filter kState keys
      loc        = scene^.objects.player.location
      updateNPCS = applyLinear
                 . over (physics._Just.speed) (clamp 20)
                 . addGravityForce loc
      !newScene  = -- set objects.player.location) (countPosition newTime2)
                 -- adjustCamera
                 over (objects.player) (friction.applyForce)
                 . over (objects.npc) (computeForces updateNPCS)
                 $ handleKeys scene
  -- print $ newScene^.campos
  endTime <- SDL.ticks
  when ((endTime - begTime) < frameTime)
    $ SDL.delay (frameTime - (endTime - begTime))
  print $ endTime - begTime
  putMVar mv newScene
  unless qpressed $ mainLoop mv window newScene

sdlMain :: IO ()
sdlMain = do
  SDL.initializeAll
  window <- SDL.createWindow "yolo" SDL.defaultWindow {
      SDL.windowOpenGL = Just SDL.defaultOpenGL
    , SDL.windowInitialSize = SDL.V2 1366 768 }
  mv <- newEmptyMVar
  dthr <- drawThread mv window
  scene <- takeMVar mv
  mainLoop mv window scene
  killThread dthr
  GL.finish
  -- SDL.glDeleteContext context
  SDL.quit
