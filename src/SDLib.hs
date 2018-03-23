{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns#-}
module SDLib where

import qualified SDL
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable ((!))
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.GL as GLRaw
import           Control.Monad
import           Foreign (sizeOf)
import           Foreign.Ptr (intPtrToPtr)
import qualified Numeric.LinearAlgebra as L
import qualified Numeric.LinearAlgebra.Data as LD
import qualified SDL.Image as SDLImage
import qualified Data.HashMap as HM
import           Numeric.LinearAlgebra (Matrix(..), (><))
import           Numeric.LinearAlgebra.Data (flatten, reshape,cmap)
import           Control.Lens
import           Debug.Trace
import           Control.Arrow
import           Number.Quaternion hiding (normalize,norm)
import           Data.Monoid

data Shaders = Shaders { _program  :: GL.Program,
                         _uniforms :: HM.Map String GL.GLint }

makeLenses ''Shaders

data Mesh = Mesh { _vertices     :: VS.Vector Float,
                   _vao          :: GL.VertexArrayObject,
                   _vbo          :: GL.BufferObject,
                   _shaders      :: Shaders,
                   _rotation     :: Maybe (L.Matrix Float),
                   _quaternion   :: T Float,
                   _texture      :: Maybe GL.TextureObject,
                   _normalMap    :: Maybe GL.TextureObject }

makeLenses ''Mesh

data PhysicsData = PhysicsData { _acc   :: L.Vector Float,
                                 _speed :: L.Vector Float }

data GameObject = GameObject { _mesh     :: Mesh,
                               _location :: VS.Vector Float,
                               _physics  :: Maybe PhysicsData,
                               _children :: [GameObject] }

data Objects =Objects { _player :: GameObject,
                        _npc    :: [GameObject],
                        _area   :: [GameObject] }

data Scene = Scene { _campos     :: L.Vector GLRaw.GLfloat,
                     _objects    :: Objects,
                     _projection :: VS.Vector GLRaw.GLfloat }

normalize :: L.Vector Float -> L.Vector Float
normalize v = let n = (1/) . realToFrac $ L.norm_2 v
               in L.scale n v

lookAt :: L.Vector Float -> L.Vector Float -> L.Matrix Float
lookAt eye at =
  let zaxis = normalize $ eye - at
      xaxis = normalize $ L.cross zaxis [0,1,0]
      yaxis = L.cross xaxis zaxis
      crd x = negate $ L.dot eye x
  in (4L.><4) [ xaxis!0,   yaxis!0,   zaxis!0,   0
              , xaxis!1,   yaxis!1,   zaxis!1,   0
              , xaxis!2,   yaxis!2,   zaxis!2,   0
              , crd xaxis, crd yaxis, crd zaxis, 1]

makeLenses ''PhysicsData

makeLenses ''GameObject

makeLenses ''Objects

makeLenses ''Scene

maxspeed = 150

-- loadShaders :: String -> String -> IO GL.Program
loadShaders vertNm fragNm uniformNms = do
  vertexShader   <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader
  vertSrc        <- BS.readFile vertNm
  fragSrc        <- BS.readFile fragNm
  GL.shaderSourceBS vertexShader $= vertSrc
  GL.shaderSourceBS fragmentShader $= fragSrc
  GL.compileShader vertexShader
  GL.shaderInfoLog vertexShader >>= print . ("Vert Log:\n"++)
  GL.compileShader fragmentShader
  GL.shaderInfoLog fragmentShader >>= print . ("Frag Log:\n"++)
  prog <- GL.createProgram
  GL.attachShader prog vertexShader
  GL.attachShader prog fragmentShader
  GL.linkProgram prog
  GL.detachShader prog vertexShader
  GL.detachShader prog fragmentShader
  uniforms <- foldM (addUniform prog) HM.empty uniformNms
  return $ Shaders prog uniforms
    where
      addUniform prog hm name = do
        GL.UniformLocation id <- GL.uniformLocation prog name
        return $ HM.insert name id hm

applyWithPhysics :: (PhysicsData -> GameObject -> GameObject)
                   -> GameObject -> GameObject
applyWithPhysics f obj = case obj^.physics of
  Just phy -> f phy obj
  Nothing  -> obj

traction = applyWithPhysics helper
  where helper phy =
          let -- spdNr = realToFrac . L.norm_2 $ phy^.speed
              force = 1
          in over (physics._Just.acc) (subtract . VS.map (*force) $ phy^.speed)

boost = applyWithPhysics helper
  where helper phy obj =
          let quat   = obj^.mesh.quaternion
              rot    = (\(a,b,c) -> [a,b,c]) . imag . flip quatConcat (qInverse quat) $ quatConcat quat (0 +:: (0,0,-1))
              force  = 30 -- exp (maxspeed - sNorm) - 1
          in set (physics._Just.acc) (VS.map (*force) rot) obj

posToMat vec = let pref = [1,0,0,0
                           ,0,1,0,0
                           ,0,0,1,0]
                in LD.reshape 4 $ pref VS.++ VS.snoc vec 1

applyForce = applyWithPhysics helper
  where helper ph obj =
         let ac    = view acc ph
             sp    = view speed ph
             newSp = sp + L.scale step ac
             move  = L.scale step newSp
         in set (physics._Just.acc) [0,0,0] . set (physics._Just.speed) newSp . over location (+move) $ obj

class Drawable d where
  draw :: d -> IO ()

instance Drawable Scene where
  draw (Scene pos obj proj) =
    let camMat     = lookAt pos $ obj^.player.location
        objectDraw = drawObject camMat proj
    in do
      mapM_ objectDraw $ obj^.player : obj^.npc ++ obj^.area

splitOn x =
  foldr (\c -> if c == x then ([]:) else over (ix 0) (c:)) [[]]

readOBJ :: FilePath -> IO (VS.Vector Float)
readOBJ fn = do
  text <- map words . lines <$> readFile fn
  let verts    = map (VS.fromList.map read.tail) . filter ((=="v").head) $ text
      normals  = map (VS.fromList.map read.tail) . filter ((=="vn").head) $ text
      faces    = (map ((\[a,_,b] -> (read a,read b)) . splitOn '/') . tail) =<< filter ((=="f").head) text
      --                 u v color       tangent bitangent
      addColor = (VS.++ [1,1,0.5,0.5,0.5,0,0,0,0,0,0])
      faceProc = subtract 1 *** subtract 1 >>> (verts!!) *** (normals!!) >>> uncurry (VS.++) >>> addColor
      result   = VS.concat $ map faceProc faces
  return result

step :: Float
step = 0.02

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture fn = do
  sdlSurf <- SDLImage.load fn
  texName <- GL.genObjectName
  SDL.V2 w h <- fmap fromIntegral <$> SDL.surfaceDimensions sdlSurf
  SDL.lockSurface sdlSurf
  texturePtr <- SDL.surfacePixels sdlSurf
  let textureSize = GL.TextureSize2D w h
      textureData = GL.PixelData GL.RGB GL.UnsignedByte texturePtr
  GL.textureBinding GL.Texture2D $= Just texName
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB' textureSize 0 textureData
  GL.textureFilter GL.Texture2D $=
    ((GL.Linear', Nothing),GL.Linear')
  SDL.unlockSurface sdlSurf
  return texName

type Event = Scene -> Scene

adjustCamera scene =
  let playPos = scene^.objects.player.location
      pointer = playPos - scene^.campos
      dist    = norm pointer
      maxDist = 5
      tooFar  = dist > maxDist
  in if tooFar then over campos (+L.scale ((dist-maxDist)/dist) pointer) scene
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
  [ 4, 0, -10
  ,-4, 0, -10
  , 0, 10, 0
  , 0, 10, 0]

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
    lVert i = (lowerPoints!!i)
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

sum3 (a,b,c) (d,e,f) = (a+d,b+e,c+f)

scale3 n (a,b,c) = (n*a,n*b,n*c)

quatConcat q0 q1 =
  let w0 = real q0
      w1 = real q1
      v0 = imag q0
      v1 = imag q1
      wr = w1 * w0 - scalarProduct v1 v0
      vr = scale3 w1 v0 `sum3` scale3 w0 v1 `sum3` crossProduct v1 v0
  in wr +:: vr

qInverse q =
  let w = real q
      v = (\(a,b,c) -> (negate a, negate b, negate c)) $ imag q
  in w +:: v


-- TO OPTIMIZE
mat3ToMat4 :: L.Matrix Float -> L.Matrix Float
mat3ToMat4 = LD.fromLists . foldr (\a b -> (a ++ [0]) : b) [[0,0,0,1]] . LD.toLists

drawObject cam proj obj = do
  GL.currentProgram $= Just (obj^.mesh.shaders.program)
  GL.bindVertexArrayObject $= Just (obj^.mesh.vao)
  passMatrix "camera" $ flatten cam
  passMatrix "proj" proj
  passMatrix "rot" . flatten . quatToMat $ obj^.mesh.quaternion
  passMatrix "pos" . flatten . posToMat  $ obj^.location
  GL.textureBinding GL.Texture2D $= obj^.mesh.texture
  GL.drawArrays GL.Triangles 0 (fromIntegral $ div (VS.length vrt) 17)
  GL.bindVertexArrayObject $= Nothing
  where
    vrt       = obj^.mesh.vertices
    quatToMat = mat3ToMat4 . LD.fromArray2D . toRotationMatrix
    passMatrix name mat =
      let uniformName = HM.findWithDefault (-1) name $ obj^.mesh.shaders.uniforms
      in VS.unsafeWith mat $
           \ptr -> GLRaw.glUniformMatrix4fv uniformName 1 0 ptr

vertexBufferData :: VS.Vector Float
vertexBufferData =
  --position        normal         UV        color          tangents
  [-1.0, 0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    1.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
   -1.0, 0.0,  1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    1.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    1.0, 0.0,  1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
   -1.0, 0.0,  1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

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
keyHandler SDL.ScancodeLeft = over (objects.player) $ turnQuatZ (step * 2)
keyHandler SDL.ScancodeRight = over (objects.player) $ turnQuatZ (negate step * 2)
keyHandler SDL.ScancodeSpace = over (objects.player) boost
keyHandler _             = id

keys :: [SDL.Scancode]
keys = [SDL.ScancodeA,
        SDL.ScancodeD,
        SDL.ScancodeW,
        SDL.ScancodeS,
        SDL.ScancodeSpace,
        SDL.ScancodeLeft,
        SDL.ScancodeRight]

mainLoop :: Float -> SDL.Window -> Scene -> IO ()
mainLoop time window scene = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  draw scene
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  kState <- SDL.getKeyboardState
  let qpressed   = any handleQuit events
      newTime = time + 0.001
      newTime2 = if newTime > 1 then 0 else newTime
      handleKeys = foldr ((.).keyHandler) id $ filter kState keys
      newScene   = set (objects.npc.ix 0.location) (countPosition newTime2)
                 . adjustCamera
                 . over (objects.player) (applyForce.traction)
                 $ handleKeys scene
  unless qpressed $ mainLoop newTime2 window newScene

flatQuad shad = createMesh vertexBufferData shad . Just

createMesh verts shad mtex = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao
    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    VS.unsafeWith verts $
      \vptr -> GL.bufferData GL.ArrayBuffer $=
                 (fromIntegral $ VS.length verts * sizeOf (0 :: Float),
                  vptr,
                  GL.StaticDraw)
    setAttrib 17 3 0 0  -- vertex location
    setAttrib 17 3 1 3  -- vertex normal
    setAttrib 17 2 2 6  -- vertex UV
    setAttrib 17 3 3 8  -- vertex color
    setAttrib 17 3 4 11 -- vertex tangent
    setAttrib 17 3 5 14 -- vertex bitangent
    GL.bindVertexArrayObject $= Nothing
    return $ Mesh verts vao vbo shad Nothing (1 +:: (0,0,0)) mtex Nothing
  where
    setAttrib size attrSize loc offset = do
      let glFloatSize = sizeOf (undefined :: GL.GLfloat)
          glLocation  = fromIntegral $ size * glFloatSize
          glOffset    = intPtrToPtr $ fromIntegral(offset * glFloatSize)
      GL.vertexAttribPointer (GL.AttribLocation loc) $=
        (GL.ToFloat,
         GL.VertexArrayDescriptor attrSize GL.Float glLocation glOffset)
      GL.vertexAttribArray (GL.AttribLocation loc) $= GL.Enabled

sdlMain :: IO ()
sdlMain = do
  SDL.initializeAll
  window <- SDL.createWindow "yolo" SDL.defaultWindow {
      SDL.windowOpenGL = Just SDL.defaultOpenGL
    , SDL.windowInitialSize = SDL.V2 1366 768 }
  context <- SDL.glCreateContext window
  GL.depthFunc $= Just GL.Less
  let uniforms = ["camera","proj","pos","rot","texSampler"] :: [String]
  shad <- loadShaders "shaders/vert.glsl" "shaders/frag.glsl" uniforms
  shadNoTex <- loadShaders "shaders/vert.glsl" "shaders/frag_no_text.glsl" uniforms
  ultraMesh <- readOBJ "media/untitled.obj"
  ultraVeh  <- readOBJ "media/vehicle.obj"
  meh       <- createMesh ultraMesh shadNoTex Nothing
  veh       <- createMesh ultraVeh shadNoTex Nothing
  crvMesh   <- createMesh curveVertices shadNoTex Nothing
  normalTex <- loadTexture "media/normal2.png"
  tex       <- loadTexture "media/grass.jpg"
  qd        <- flatQuad shad tex
  let quads = map (\(x,z) -> GameObject qd [x, -1, z] Nothing []) [(x,z) | x <- [-30,-28..30], z <- [-50,-48..0]]
      n = 0.1
      f = 100
      t = 1 / tan ((pi/3) / 2)
      as= 1366/768
      proj = (4><4) [ t / as, 0,           0, 0
                    , 0,      t,           0, 0
                    , 0,      0, (f+n)/(n-f),(2*f*n)/(n-f)
                    , 0,      0,          -1, 0]
      camPos = [ 0, 10,20]
      emptyPhysics = (Just $ PhysicsData [0,0,0] [0,0,0])
      vehOb = GameObject veh [4,0,-10] emptyPhysics []
      mehOb = GameObject meh [0,0.1,-3] emptyPhysics []
      curve = GameObject crvMesh [0,0,0] Nothing []
      scene = Scene camPos (Objects mehOb [vehOb] (curve:quads)) (flatten . L.tr $ proj)
  mainLoop 0 window scene
  GL.finish
  SDL.glDeleteContext context
  SDL.quit
