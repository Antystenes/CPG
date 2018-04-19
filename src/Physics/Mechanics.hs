{-# LANGUAGE OverloadedLists #-}
module Physics.Mechanics where

import           Numeric.LinearAlgebra (Matrix, Vector, (><), flatten, scale, dot, (#>), cross, cmap)
import qualified Number.Quaternion as Q
import           Control.Lens

import Data.PhysicsData
import Data.GameObject
import Data.Scene
import Data.Mesh


import Utils(step,norm)
import Utils.Quaternions

putForce :: Vector Float -> Vector Float -> GameObject -> GameObject
putForce point force obj =
          let p    = point - obj^.location
              lin  = scale (dot force p) force
              ang  = cross force p
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


applyForce = applyWithPhysics helper
  where helper ph obj =
         let ac    = ph^.acc
             tq    = ph^.torque
             sp    = ph^.speed
             om    = ph^.angularS
             newSp = sp + scale step ac
             inInv = ph^.inertiaInv
             newOm = om + (inInv #> scale step tq)
             quatD = qvMul (scale step newOm) $ obj^.mesh.quaternion
             newQ  = Q.normalize . addQuat quatD $ obj^.mesh.quaternion
             inInD = quatToLInv (ph^.inertiaD) newQ
             move  = scale step newSp
         in set (physics._Just.angularS) newOm
          . set (physics._Just.torque) [0,0,0]
          . set (physics._Just.acc) [0,0,0]
          . set (physics._Just.speed) newSp
          . set (physics._Just.inertiaInv) inInD
          . set (mesh.quaternion) newQ
          . over location (+move)
          $ obj


applyWithPhysics :: (PhysicsData -> GameObject -> GameObject)
                   -> GameObject -> GameObject
applyWithPhysics f obj = case obj^.physics of
  Just phy -> f phy obj
  Nothing  -> obj


maxspeed = 50


yolo :: Float -> Vector Float -> Vector Float
yolo maxVal vec =
  let vecNorm = norm $ vec
      coeff   = (vecNorm/maxVal)**(1/2)
  in scale (0 - max 1 coeff) vec

traction = applyWithPhysics helper
  where helper phy =
          let maxspeed  = 100000000
              maxtorque = 10000
              coeff1    = scale step . yolo maxspeed $ phy^.speed
              coeff2    = scale step . yolo maxtorque $ phy^.angularS
          in over (physics._Just.angularS) (+ coeff2)
            . over (physics._Just.speed) (+ coeff1)

boost = applyWithPhysics helper
  where helper phy obj =
          let quat   = obj^.mesh.quaternion
              rot    = rotateWithQuat quat [0,0,-1]
                -- (\(a,b,c) -> [a,b,c]) . imag . flip quatConcat (qInverse quat) $ quatConcat quat (0 +:: (0,0,-1))
              force  = 40 -- exp (maxspeed - sNorm) - 1
          in over (physics._Just.acc) (+cmap (*force) rot) obj
