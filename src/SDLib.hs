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


import           Utils (step, projectionMatrix,normalize,clampSpeed,norm)

import           OBJReader
import           Data.Shaders (loadShaders)

import           Data.Mesh    ( loadTexture
                              , createMesh
                              , flatQuad
                              , quaternion)
import           Data.GameObject
import           Physics.Collision
import           Data.Scene
import           Data.PhysicsData (acc,speed)
import           Data.SkyBox
import           Physics.Mechanics
import           Tasks.Swarm
import           EventHandler
import           Utils.Quaternions

frameTime = round $ 1000/60


-- drawThread ::

debugPrint (GL.DebugMessage _ GL.DebugTypeError _ _ err) = putStrLn err
debugPrint _ = return ()

setup = do
  SDL.swapInterval $= SDL.SynchronizedUpdates
  GL.depthFunc $= Just GL.Less
  -- GL.cullFace $= Just GL.Front
  -- GL.debugMessageCallback $= Just debugPrint
  -- GL.debugOutput $= GL.Enabled


drawThread mv window = forkOS $ do
  context <- SDL.glCreateContext window
  setup
  let uniforms = ["camera","proj","pos","rot","texSampler","normSampler"] :: [String]
  shad <- loadShaders "shaders/vert.glsl" "shaders/frag.glsl" uniforms
  shadNoTex <- loadShaders "shaders/vert.glsl" "shaders/frag_no_text.glsl" uniforms
  ultraMesh <- readOBJ "media/sphere.obj"
  ultraVeh  <- readOBJ "media/cube.obj"
  vehPrim   <- readBoxFromObj "media/cube.obj"
  let miniVeh = ultraMesh
  meh       <- createMesh ultraVeh shadNoTex Nothing Nothing
  veh       <- createMesh ultraMesh shadNoTex Nothing Nothing
  -- crvMesh   <- createMesh curveVertices shadNoTex Nothing
  tex       <- loadTexture "media/bricks_dif.png"
  normalTex <- loadTexture "media/bricks_norm.png"
  sbox      <- loadSkyBox
  qd        <- flatQuad shad tex normalTex
  let quads =
        V.fromList .
        map (\(x,z) -> GameObject qd [x, -2.5, z] Nothing [] (Plane [0,1,0] (-2.5))) $
        [(x,z) | x <- [-30,-26..30], z <- [-30,-26..30]]
      camPos = [ 0, 10,20]
      vehOb pos = initializePhysics (1/10) $
        GameObject meh pos Nothing [] vehPrim
      sphOb pos = initializePhysics (1/10) .
                  GameObject veh pos Nothing [] $
                  Sphere 1 (LD.ident 4)
      dim = 1
      sep = 3
      vehs = -- V.fromList [vehOb [0.5,1,-5]]
        -- V.fromList []
                    -- initializePhysics (1/10) .
                    -- GameObject veh [0.5,1,-5] Nothing [] $
                    -- Sphere 1 (LD.ident 4)]
        V.fromList [sphOb [0,2,sep*z] | z <- [0..dim]]
      mehOb =
        -- initializePhysics (1/100) .
        --             GameObject veh [0, 0, 10] Nothing [] $
        --             Sphere 1 (LD.ident 4)
        initializePhysics (1/10) $
        GameObject meh [0,0,10] Nothing [] vehPrim
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
      -- updateNPCS = applyLinear
      --            . over (physics._Just.speed) (clamp 20)
      --            . addGravityForce loc
      !newScene  = -- set objects.player.location) (countPosition newTime2)
                 adjustCamera .
                 over objects resolveObjectCollisions .
                 traverseObjects (floorReaction . applyForce . friction) .
                 traverseObjects gravityF $
                 -- . over (objects.npc) (computeForces updateNPCS)
                 handleKeys scene
  -- print $ newScene^.campos
  endTime <- SDL.ticks
  when ((endTime - begTime) < frameTime)
    $ SDL.delay (frameTime - (endTime - begTime))
  print $ endTime - begTime
  print $ scene^.objects.player
  let cp = prepareCPrim $ scene^.objects.player
      p  = V.toList $ _cpoints cp
      d  = [norm (a- b) | a <- p, b <- p]
  print $ p
  print d
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
