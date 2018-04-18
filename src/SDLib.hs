{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}

module SDLib where

import qualified SDL
import qualified Data.Vector.Storable as VS
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.GL as GLRaw
import           Control.Monad
import           Foreign (sizeOf)
import           Foreign.Ptr (intPtrToPtr, IntPtr(..))
import qualified Numeric.LinearAlgebra as L
import           Numeric.LinearAlgebra (Matrix, (><))
import           Numeric.LinearAlgebra.Data (flatten)
import qualified Numeric.LinearAlgebra.Data as LD
import qualified Data.HashMap               as HM
import           Control.Lens      hiding (indices)
import           Debug.Trace
import           Control.Arrow
import           Number.Quaternion hiding (normalize,norm)
import qualified Number.Quaternion as Q
import           Data.Monoid
import           Utils (lookAt,glTexture)

import           OBJReader
import           Data.Shaders (Shaders(..),
                               program,
                               uniforms,
                               loadShaders)

import           Data.Mesh    ( Mesh(..)
                              , loadTexture
                              -- lenses
                              , vertices
                              , indices
                              , vao
                              , vbo
                              , ebo
                              , shaders
                              , rotation
                              , quaternion
                              , texture
                              , normalMap)

import           Data.PhysicsData ( PhysicsData(..)
                                  , startPhysics
                                  , acc, speed, inertiaD, inertiaInv
                                  , angularS, torque)

import           Utils.Quaternions

data GameObject = GameObject {
  _mesh     :: Mesh,
  _location :: VS.Vector Float,
  _physics  :: Maybe PhysicsData,
  _children :: [GameObject] }

data Objects = Objects {
  _player :: GameObject,
  _npc    :: [GameObject],
  _area   :: [GameObject] }

data Scene = Scene {
  _campos     :: L.Vector GLRaw.GLfloat,
  _objects    :: Objects,
  _projection :: VS.Vector GLRaw.GLfloat }

makeLenses ''GameObject

makeLenses ''Objects

makeLenses ''Scene


initializePhysics obj =
  let quat = obj^.mesh.quaternion
  in set physics (Just $ startPhysics quat) obj

maxspeed = 50

traverseObjects :: (GameObject -> GameObject) -> Scene -> Scene
traverseObjects =
  over (objects.player) &&& over (objects.npc.traverse) >>> uncurry (.)

applyWithPhysics :: (PhysicsData -> GameObject -> GameObject)
                   -> GameObject -> GameObject
applyWithPhysics f obj = case obj^.physics of
  Just phy -> f phy obj
  Nothing  -> obj

yolo :: Float -> VS.Vector Float -> VS.Vector Float
yolo maxVal vec =
  let vecNorm = realToFrac . L.norm_2 $ vec
      coeff   = (vecNorm/maxVal)**(1/2)
  in L.scale (0 - max 1 coeff) vec

traction = applyWithPhysics helper
  where helper phy =
          let maxspeed  = 100000000
              maxtorque = 10000
              coeff1    = L.scale step . yolo maxspeed $ phy^.speed
              coeff2    = L.scale step . yolo maxtorque $ phy^.angularS
          in over (physics._Just.angularS) (+ coeff2)
            . over (physics._Just.speed) (+ coeff1)

boost = applyWithPhysics helper
  where helper phy obj =
          let quat   = obj^.mesh.quaternion
              rot    = rotateWithQuat quat [0,0,-1]
                -- (\(a,b,c) -> [a,b,c]) . imag . flip quatConcat (qInverse quat) $ quatConcat quat (0 +:: (0,0,-1))
              force  = 40 -- exp (maxspeed - sNorm) - 1
          in over (physics._Just.acc) (+VS.map (*force) rot) obj

putForce :: VS.Vector Float ->VS.Vector Float -> GameObject -> GameObject
putForce point force obj =
          let p    = point - obj^.location
              lin  = L.scale (L.dot force p) force
              ang  = L.cross force p
          in over (physics._Just.acc) (+lin)
           . over (physics._Just.torque) (+ang)
           $ obj

putForceOnTip vec obj =
  let quat = obj^.mesh.quaternion
      tip  = rotateWithQuat quat [0,0,-1] + obj^.location
  in putForce tip vec obj

putForceOnTipLoc vec obj =
  let quat = obj^.mesh.quaternion
      f    = rotateWithQuat quat vec
  in putForceOnTip f obj

posToMat vec = let pref = [1,0,0,0
                           ,0,1,0,0
                           ,0,0,1,0]
                in LD.reshape 4 $ pref VS.++ VS.snoc vec 1


applyForce = applyWithPhysics helper
  where helper ph obj =
         let ac    = ph^.acc
             tq    = ph^.torque
             sp    = ph^.speed
             om    = ph^.angularS
             newSp = sp + L.scale step ac
             inInv = ph^.inertiaInv
             newOm = om + (inInv L.#> L.scale step tq)
             quatD = qvMul (L.scale step newOm) $ obj^.mesh.quaternion
             newQ  = Q.normalize . addQuat quatD $ obj^.mesh.quaternion
             inInD = quatToLInv (ph^.inertiaD) newQ
             move  = L.scale step newSp
         in set (physics._Just.angularS) newOm
          . set (physics._Just.torque) [0,0,0]
          . set (physics._Just.acc) [0,0,0]
          . set (physics._Just.speed) newSp
          . set (physics._Just.inertiaInv) inInD
          . set (mesh.quaternion) newQ
          . over location (+move)
          $ obj

class Drawable d where
  draw :: d -> IO ()

instance Drawable Scene where
  draw (Scene pos obj proj) =
    let camMat     = lookAt pos $ obj^.player.location
        objectDraw = drawObject camMat proj
    in mapM_ objectDraw $ obj^.player : obj^.npc ++ obj^.area

step :: Float
step = 0.02

type Event = Scene -> Scene

adjustCamera scene =
  let playPos = scene^.objects.player.location
      pointer = playPos - scene^.campos
      dist    = norm pointer
      maxDist = 5
      tooFar  = dist > maxDist
      movement= L.scale ((dist-maxDist)/dist) pointer
  in if tooFar then over campos (+movement) scene
     else scene

moveHor :: Float -> Scene -> Scene
moveHor s = moveCamera s 0 0

moveLeftCamera :: Scene -> Scene
moveLeftCamera = moveHor step

moveRightCamera :: Scene -> Scene
moveRightCamera = moveHor (-step)

rotZMatrix t = (4L.><4) [   cos t, 0,-(sin t), 0
                         ,       0, 1,       0, 0
                         ,  sin t , 0,   cos t, 0
                         ,       0, 0,       0, 1]


hermiteMatrix :: Matrix Float
hermiteMatrix = (4L.><4)
  [ 2,-2, 1, 1
  ,-3, 3,-2,-1
  , 0, 0, 1, 0
  , 1, 0, 0, 0 ]

curvePoints :: Matrix Float
curvePoints = (4L.><3)
  [ 20, 0, -10
  , 20, 0, -10
  , 100, 100, 0
  , 100, -100, 0]

countPosition :: Float -> LD.Vector Float
countPosition t = flatten $ (1><4) [t^3,t^2,t^1,1] <> hermMat
  where
    hermMat = hermiteMatrix <> curvePoints

lowerPoints :: [VS.Vector Float]
lowerPoints = map countPosition  [0,0.05..0.999]

curveVertices :: VS.Vector Float
curveVertices = VS.concat . map (VS.++ vData) $ quad =<< [0..length lowerPoints - 2]
  where
    lowerTri i = [lVert $ i+1,lVert i,uVert i] :: [VS.Vector Float]
    upperTri i = [lVert $ i+1, uVert i, uVert $ i+1] :: [VS.Vector Float]
    quad i = lowerTri i ++ upperTri i
    lVert i = lowerPoints!!i
    uVert i = (lowerPoints!!i) + ([0,1,0] :: VS.Vector Float)
    vData = [0,0,1,0,0,1,0,1,0,0,0,0,0,0] :: VS.Vector Float

-- turnZ t = over (mesh.rotation) (rotZMatrix t L.<>)

turnQuatZ = over (mesh.quaternion) . quatConcat . rotQuatZ
  where
    rotQuatZ t = cos (t/2) +:: (0,sin $ t / 2,0)

moveCameraV = over campos . (+)

moveCamera :: Float -> Float -> Float -> Scene -> Scene
moveCamera x y z = moveCameraV [x,y,z]

moveDepth :: Float -> Scene -> Scene
moveDepth = moveCamera 0 0

moveForwardCamera :: Scene -> Scene
moveForwardCamera = moveDepth step

moveBackCamera :: Scene -> Scene
moveBackCamera = moveDepth (-step)

norm :: L.Vector Float -> Float
norm = realToFrac . L.norm_2

gravityCenter :: L.Vector Float
gravityCenter = [0,0,0]

addGravityForce :: VS.Vector Float -> GameObject -> GameObject
addGravityForce gravCent obj =
  let direction = gravCent - obj^.location
      norm = realToFrac $ L.norm_2 direction
      force = L.scale (10/norm) direction
  in over (physics._Just.acc) (+force) obj

-- TO OPTIMIZE

drawObject cam proj obj = do
  GL.currentProgram $= Just (obj^.mesh.shaders.program)
  GL.bindVertexArrayObject $= Just (obj^.mesh.vao)
  passMatrix "camera" $ flatten cam
  passMatrix "proj" proj
  passMatrix "rot" . flatten . quatToMat $ obj^.mesh.quaternion
  passMatrix "pos" . flatten . posToMat  $ obj^.location

  passTexture "texSampler" 1 $ obj^.mesh.texture
  passTexture "normSampler" 2 $ obj^.mesh.normalMap

  GL.drawElements GL.Triangles indLen GL.UnsignedInt (intPtrToPtr $ IntPtr 0)

  GL.bindVertexArrayObject $= Nothing
  where
    indLen    = fromIntegral . VS.length $ obj^.mesh.indices
    uniformName name =
      HM.findWithDefault (-1) name $ obj^.mesh.shaders.uniforms
    passMatrix name mat =
      VS.unsafeWith mat $
         GLRaw.glUniformMatrix4fv (uniformName name) 1 0
    passTexture name pos mtex = do
      GLRaw.glActiveTexture $ glTexture pos
      GL.textureBinding GL.Texture2D $= mtex
      GL.uniform (GL.UniformLocation $ uniformName name) $= GL.TextureUnit pos



vertexBufferData :: (VS.Vector Float, VS.Vector GL.BaseInstance)
vertexBufferData =
  let v = --position        normal         UV        color          tangents
          [-5.0, 0.0, -5.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0,
            5.0, 0.0, -5.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0,
           -5.0, 0.0,  5.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0,
            5.0, 0.0,  5.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 1.0]
      i = [0,1,2,1,3,2]
  in (v,i)

handleQuit :: SDL.Event -> Bool
handleQuit event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEv ->
    SDL.keyboardEventKeyMotion keyEv == SDL.Pressed &&
    SDL.keysymKeycode (SDL.keyboardEventKeysym keyEv) == SDL.KeycodeEscape
  _ -> False

keyHandler :: SDL.Scancode -> Scene -> Scene
keyHandler SDL.ScancodeA = moveLeftCamera
keyHandler SDL.ScancodeD = moveRightCamera
keyHandler SDL.ScancodeW = moveForwardCamera
keyHandler SDL.ScancodeS = moveBackCamera
keyHandler SDL.ScancodeLeft = over (objects.player) $ putForceOnTipLoc [1,0,0] -- turnQuatZ (step * 2) --   -- $
keyHandler SDL.ScancodeRight = over (objects.player) $ putForceOnTipLoc [-1,0,0]  -- turnQuatZ (negate step * 2) --  -- $
keyHandler SDL.ScancodeDown = over (objects.player) $ putForceOnTipLoc [0,1,0] -- turnQuatZ (step * 2) --   -- $
keyHandler SDL.ScancodeUp = over (objects.player) $ putForceOnTipLoc [0,-1,0]  -- turnQuatZ (negate step * 2) --  -- $
keyHandler SDL.ScancodeSpace = over (objects.player) boost
keyHandler _             = id

keys :: [SDL.Scancode]
keys = [SDL.ScancodeA,
        SDL.ScancodeD,
        SDL.ScancodeW,
        SDL.ScancodeS,
        SDL.ScancodeSpace,
        SDL.ScancodeLeft,
        SDL.ScancodeRight,
        SDL.ScancodeDown,
        SDL.ScancodeUp]

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
--                  . over (objects.player) (applyForce.traction)
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

flatQuad shad tex norm =
  createMesh vertexBufferData shad (Just tex) (Just norm)

createMesh verts shad mtex norm = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao
    ebo <- createAndBindArrayBuffer GL.ElementArrayBuffer $ snd verts
    vbo <- createAndBindArrayBuffer GL.ArrayBuffer $ fst verts

    setAttrib 17 3 "vertexPosition_modelspace" 0  -- vertex location
    setAttrib 17 3 "vertexNormal" 3  -- vertex normal
    setAttrib 17 2 "vertexUV" 6  -- vertex UV
    setAttrib 17 3 "vertex_col" 8  -- vertex color
    setAttrib 17 3 "vertex_tangent" 11 -- vertex tangent
    setAttrib 17 3 "vertex_bitangent" 14 -- vertex bitangent
    GL.bindVertexArrayObject $= Nothing
    return $ uncurry Mesh verts vao vbo ebo shad Nothing (1 +:: (0,0,0)) mtex norm
  where
    setAttrib size attrSize name offset = do
      let glFloatSize = sizeOf (undefined :: GL.GLfloat)
          glLocation  = fromIntegral $ size * glFloatSize
          glOffset    = intPtrToPtr $ fromIntegral(offset * glFloatSize)
      loc <- GL.get $ GL.attribLocation (shad^.program) name
      GL.vertexAttribPointer loc $=
        (GL.ToFloat,
         GL.VertexArrayDescriptor attrSize GL.Float glLocation glOffset)
      GL.vertexAttribArray loc $= GL.Enabled

createAndBindArrayBuffer
  :: forall a. VS.Storable a =>
  GL.BufferTarget
  -> VS.Vector a
  -> IO GL.BufferObject
createAndBindArrayBuffer arrayType arrayData = do
    bufferObject <- GL.genObjectName
    GL.bindBuffer arrayType $= Just bufferObject
    sendData arrayType arrayData
    return bufferObject

sendData :: forall a. VS.Storable a => GL.BufferTarget -> VS.Vector a -> IO ()
sendData t d =
  VS.unsafeWith d $
    \vptr -> GL.bufferData t $=
               (fromIntegral $ VS.length d * sizeOf (undefined :: a),
                vptr,
                GL.StaticDraw)

objectNeighbours obj objs =
  let loc = obj^.location
      maxDist = 3
  in filter ((maxDist >) . L.norm_2 . (loc -) . view location) objs

separationsForce obj neighb =
  let loc = obj^.location
      minDist = 1 :: Float
      checkVec v =
        let n = realToFrac $ L.norm_2 v
        in if minDist > n && n > 0.000001
           then L.scale 1 v :: VS.Vector Float
           else 0
  in
    foldr ((+).checkVec) 0
    . map ((loc-) . view location)
    $ neighb

addSeparationForce neighb obj =
  let sepForce = separationsForce obj neighb
  in over (physics._Just.acc) (+sepForce) obj

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
