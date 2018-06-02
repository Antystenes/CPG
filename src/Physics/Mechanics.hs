{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
module Physics.Mechanics where

import           Numeric.LinearAlgebra (Matrix, Vector, scale, (#>), cross, cmap,dot, ident, tr, (<>))
import qualified Number.Quaternion as Q
import           Control.Lens hiding ((<.>))
import qualified Data.Vector.Storable as VS
import           Control.Arrow hiding ((<+>))
import           Debug.Trace
import           Data.Maybe (fromMaybe)


import Data.PhysicsData
import Data.GameObject
import Data.Mesh


import Utils(step,norm)
import Utils.Quaternions

infixr 1 <+>
(<+>) :: Vector Float -> Vector Float -> Vector Float
(<+>) = VS.zipWith (+)

(<*>) :: Vector Float -> Vector Float -> Vector Float
(<*>) = VS.zipWith (*)

(<.>) :: Vector Float -> Vector Float -> Float
(<.>) = (VS.sum .) . VS.zipWith (*)

putForceLoc :: Vector Float -> Vector Float -> GameObject -> GameObject
putForceLoc point force obj =
          let lin  = scale (force <.> point) force
              ang  = cross force point
          in over (physics._Just.acc) (<+>lin)
           . over (physics._Just.torque) (<+>ang)
           $ obj


putForce :: Vector Float -> Vector Float -> GameObject -> GameObject
putForce point force obj =
          let p    = point - obj^.location
          in putForceLoc p force obj

putForceOnMid = over (physics._Just.acc) . (+)

putForceOnTip vec obj =
  let quat = obj^.mesh.quaternion
      tip  = rotateWithQuat quat [0,0,-0.1] + obj^.location
  in putForce tip vec obj

putForceOnTipLoc vec obj =
  let quat = obj^.mesh.quaternion
      f    = rotateWithQuat quat vec
  in putForceOnTip f obj

putForceOnSide vec obj =
  let quat = obj^.mesh.quaternion
      tip  = rotateWithQuat quat [1,0,0] <+> obj^.location
  in putForce tip vec obj

putForceOnSideLoc vec obj =
  let quat = obj^.mesh.quaternion
      f    = rotateWithQuat quat vec
  in putForceOnSide f obj

addScaled
  :: Setter GameObject GameObject (VS.Vector Float) (VS.Vector Float)
  -> Lens PhysicsData PhysicsData (VS.Vector Float) (VS.Vector Float)
  -> GameObject -> GameObject
addScaled lens1 lens2 = applyWithPhysics helper
  where helper ph = over lens1 (<+> VS.map (*step) (ph^.lens2))

applySpeed = addScaled location speed

gForce :: Vector Float
gForce = [0,9,0]

gravityF = over (physics._Just.acc) (subtract gForce)

gravCoeff = scale step gForce

getVec :: Lens PhysicsData PhysicsData (VS.Vector Float) (VS.Vector Float) -> GameObject -> VS.Vector Float
getVec l = fromMaybe [0,0,0] . preview (physics._Just.l)

getMat :: Lens PhysicsData PhysicsData (Matrix Float) (Matrix Float) -> GameObject -> Matrix Float
getMat l = fromMaybe (ident 3) . preview (physics._Just.l)

floatOnGround obj =
  let floorNormal = [0,1,0]
      loc       = obj^.location
      d         = dot floorNormal loc + 2
      f         = negate (2-d)^2
  in if d < 1
  then putForceOnMid (scale f gForce) obj
  else obj

floorReaction obj =
  let floorHeight = [0,-2,0]
      floorNormal = [0,1,0] :: Vector Float
      vert      = obj^.mesh.vertices
      loc       = obj^.location
      r         = quatToMat3 $ obj^.mesh.quaternion
      translate = (+loc) . subtract floorHeight
      collides = traceShowId
               . filter ((<0) . snd)
               . map (id &&& (dot floorNormal . translate) . (tr r #>))
               $ vert
      minD :: Float
      minD = negate . foldr min 0 . map snd $ collides
      response (point, depth) =
        let
          vrel = dot floorNormal $ getVec speed obj + cross (getVec angularS obj) point
          aobj = dot floorNormal $ cross (i #> cross point floorNormal) point
          i    = tr r <> getMat inertiaInv obj <> r
          j    = negate 1 * vrel/(1+aobj)
          force = scale j floorNormal
          torque = getMat inertiaInv obj #> cross point force
        in (force, torque)
      (f,t) = foldr (\(a,b) (c,d) -> (a+c,b+d)) ([0,0,0],[0,0,0])
            . map response
            $ collides
      n = fromIntegral $ length collides
  in if n /= 0
  then over (physics._Just.angularS) (+ scale (1/n) t)
     . over (physics._Just.speed) (+ scale (1/n) f)
     . over (location) (+ scale minD floorNormal)
     $ obj
  else obj

applyAcc = addScaled (physics._Just.speed) acc

applyForce = applyWithPhysics helper
  where helper ph obj =
         let tq    = ph^.torque
             om    = ph^.angularS
             inInv = ph^.inertiaInv
             newOm = om <+> (inInv #> scale step tq)
             quatD = qvMul (scale step newOm) $ obj^.mesh.quaternion
             newQ  = Q.normalize . addQuat quatD $ obj^.mesh.quaternion
             inInD = quatToLInv (ph^.inertiaD) newQ
         in set (physics._Just.torque) [0,0,0]
          . set (physics._Just.acc) [0,0,0]
          . applySpeed
          . floorReaction
          . applyAcc
          -- . floatOnGround
          . set (physics._Just.angularS) newOm
          . set (physics._Just.inertiaInv) inInD
          . set (mesh.quaternion) newQ
          . gravityF
          $ obj

applyLinear = set (physics._Just.torque) [0,0,0]
              . set (physics._Just.acc) [0,0,0]
              . applySpeed
              -- . floorReaction
              . applyAcc
              -- . gravityF

applyWithPhysics :: (PhysicsData -> GameObject -> GameObject)
                   -> GameObject -> GameObject
applyWithPhysics f obj = case obj^.physics of
  Just phy -> f phy obj
  Nothing  -> obj


maxspeed = 50


yolo :: Float -> Vector Float -> Vector Float
yolo maxVal vec =
  let vecNorm = norm vec
      coeff   = (vecNorm/maxVal)**(1/2)
  in scale (0 - max 1 coeff) vec

friction = applyWithPhysics helper
  where helper phy =
          let maxspeed  = 100000000
              maxtorque = 10000
              coeff1    = scale step . yolo maxspeed $ phy^.speed
              coeff2    = scale step . yolo maxtorque $ phy^.angularS
          in over (physics._Just.angularS) (<+> coeff2)
            . over (physics._Just.speed) (<+> coeff1)

boost = applyWithPhysics helper
  where helper phy obj =
          let quat   = obj^.mesh.quaternion
              rot    = rotateWithQuat quat [0,0,-1]
                -- (\(a,b,c) -> [a,b,c]) . imag . flip quatConcat (qInverse quat) $ quatConcat quat (0 +:: (0,0,-1))
              force  = 40 -- exp (maxspeed - sNorm) - 1
          in over (physics._Just.acc) (<+>cmap (*force) rot) obj
