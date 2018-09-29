{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
module Physics.Mechanics where

import           Numeric.LinearAlgebra (Matrix, Vector, scale, (#>), cross, cmap,dot, ident, tr, (<>), norm_2)
import qualified Number.Quaternion as Q
import           Control.Lens hiding ((<.>))
import qualified Data.Vector.Storable as VS
import           Control.Arrow hiding ((<+>))
import           Debug.Trace
import           Data.Maybe (fromMaybe)
import           Data.Function (on)

import Data.PhysicsData
import Data.GameObject
import Data.Mesh
import Physics.Collision
import qualified Data.Vector as V
import Data.List.Lens

import Utils(step,norm)
import Utils.Quaternions

infixr 1 <+>
(<+>) :: Vector Float -> Vector Float -> Vector Float
(<+>) = VS.zipWith (+)

-- (<*>) :: Vector Float -> Vector Float -> Vector Float
-- (<*>) = VS.zipWith (*)

(<.>) :: Vector Float -> Vector Float -> Float
(<.>) = (VS.sum .) . VS.zipWith (*)

addMaybeForce :: Maybe (Vector Float) -> Maybe PhysicsData -> Maybe PhysicsData
addMaybeForce mf mph = over acc . (<+>) <$> mf <*> mph

putForceLoc :: Vector Float -> Vector Float -> GameObject -> GameObject
putForceLoc point force obj =
          let m    = preview (physics._Just.massInv) obj
              lin  = flip scale (scale (force <.> point) force) <$> m
              ang  = cross force point
          in over physics (addMaybeForce lin)
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
      f         = negate (2-d)^(2 :: Int)
  in if d < 1
  then putForceOnMid (scale f gForce) obj
  else obj

calculateRelPos contact obj = contact^.contactPoint - obj^.location

linearInertia obj =
  fromMaybe 0 $ obj^?physics._Just.massInv

adjustMove relPos p@(l, an) =
  let limit = norm relPos * 0.1
      newAn = signum an * limit
  in if abs an > limit
     then (l + an - newAn,newAn)
     else p

collisionMoves :: Contact -> (GameObject, Vector Float) -> (GameObject, Vector Float) -> ((Float, Vector Float), (Float, Vector Float))
collisionMoves contact p1 p2 =
  let ang1 = angularInertia contact p1
      lin1 = linearInertia $ fst p1
      ang2 = angularInertia contact p2
      lin2 = linearInertia $ fst p2
      coef = contact^.penetration / (ang1 + ang2 + lin1 + lin2)
      (fli1, fan1) = adjustMove (snd p1) (coef*lin1, coef*ang1)
      (fli2, fan2) = adjustMove (snd p2) (coef*lin2, coef*ang2)
      finang1 = getRotationPM contact p1 fan1 ang1
      finang2 = getRotationPM contact p1 fan2 ang2
  in ((fli1, finang1),(fli2, finang2))

getRotationPM :: Contact -> (GameObject, Vector Float) -> Float -> Float -> Vector Float
getRotationPM contact (obj, relPos) ang angIn =
  if angIn == 0
  then [0,0,0]
  else
    scale (ang) .
    scale (1/angIn) $
    getInertiaInv obj #> cross relPos (contact^.contactNormal)

applyMoves :: Contact -> (Float, Vector Float) -> GameObject -> GameObject
applyMoves contact (lin, ang) =
  over (mesh.quaternion) (qvMul ang) .
  over location (+ scale lin (contact^.contactNormal))

calculateDeltaVel :: Contact -> (GameObject, Vector Float) -> Float
calculateDeltaVel contact p@(obj, _) =
  case obj^.physics of
    Just pd ->
      (pd^.massInv +) $
      angularInertia contact p
      -- dot (contact^.contactNormal) .
      -- flip cross relPos .
      -- ((pd^.inertiaInv) #>) .
      -- flip cross (contact^.contactNormal) $ relPos
    Nothing -> 0

angCoeff = 1

angularInertia :: Contact ->  (GameObject, Vector Float) -> Float
angularInertia contact (obj, relPos) =
  case obj^.physics of
    Just pd ->
--      (*angCoeff) .
      dot (contact^.contactNormal) .        -- get component of speed pointing along the normal
      (\angS -> cross angS relPos) .        -- get linear speed of pt
      ((pd^.inertiaInv) #>) $               -- Get ang speed
      cross relPos (contact^.contactNormal) -- for unit impulse
    Nothing -> 0

calculateClosingVel :: Contact -> (GameObject, Vector Float) -> Float
calculateClosingVel contact (obj, relPos) =
  case obj^.physics of
    Just pd ->
      -- (makeContactBasis contact #>) .
      dot (contact^.contactNormal) .
      (pd^.speed +) . cross (pd^.angularS) $ relPos
    Nothing -> 0

calculateImpulse :: Contact -> (GameObject, Vector Float) -> (GameObject, Vector Float) -> Vector Float
calculateImpulse contact o1 o2 =
  let deltaV = (("Delta: "++) . show >>= trace) $
               ((+) `on` calculateDeltaVel contact) o1 o2
      closiV = (("Closing: "++) . show >>= trace) $
               ((+) `on` calculateClosingVel contact) o1 o2
      contV  = negate $ closiV
      restitution = if abs contV < 2
                    then 0.0
                    else 0.0
      impulse = -- if signum contV /= signum (contact^.sign)
                -- then
                  scale (contV * (1.0 + restitution)/deltaV) $ contact^.contactNormal
                -- else [0,0,0]
  in impulse

applyImpulse :: Vector Float -> (GameObject, Vector Float) -> GameObject
applyImpulse impulse (obj, relPos) =
  let vChange = -- impulse
        (("Linear impulse: "++) . show >>= trace) .
        fromMaybe [0,0,0] $
        flip scale impulse <$> (obj^?physics._Just.massInv)
      rotChange = (("Angular impulse: "++) . show >>= trace) .
--        scale angCoeff .
        fromMaybe [0,0,0] $
        (#> cross impulse relPos) <$> (obj^?physics._Just.inertiaInv)
  in over (physics._Just.speed) (+ vChange) .
     over (physics._Just.angularS) (+ rotChange) $ obj



resolveCollision :: (Int, Int) -> [Contact] -> V.Vector GameObject -> V.Vector GameObject
resolveCollision (ix1, ix2) (contact:cs) objs =
  let obj1 = objs V.! ix1
      obj2 = objs V.! ix2
      movement = scale (contact^.penetration*contact^.sign) $
                 contact^.contactNormal
      move = case obj1^.physics of
               Just _ -> over (ix 0._2.location) (+movement)
               Nothing -> over (ix 1._2.location) (subtract movement)
      relPos1 = (("RelPos1: "++) . show >>= trace) $ calculateRelPos contact obj1
      relPos2 = (("RelPos2: "++) . show >>= trace) $ calculateRelPos contact obj2
      impulse = (("Impulse: "++) . show >>= trace) .
                scale (contact^.sign) $
                calculateImpulse contact (obj1, relPos1) (obj2, relPos2)
      (m1, m2) = collisionMoves contact (obj1, relPos1) (obj2, relPos2)
      update i p =
        -- applyMoves contact p .
        applyImpulse i
      updates = move $
                [(ix1, update (impulse) m1 (obj1, relPos1))
                ,(ix2, update (-impulse) m2 (obj2, relPos2))]
      newObjs = V.unsafeUpd objs updates
      check   = V.unsafeIndex newObjs
      -- newColls =
      --   take (length cs) $
      --   detectCollision (prepareCPrim $ check ix1) (prepareCPrim $ check ix2)^.contacts
  in
    resolveCollision (ix1, ix2) cs newObjs
resolveCollision _ [] objs = objs


floorReaction obj =
  let floorHeight = [0,-2,0]
      floorNormal = [0,1,0] :: Vector Float
      vert      = obj^.mesh.vertices
      loc       = obj^.location
      r         = quatToMat3 $ obj^.mesh.quaternion
      m         = (1/) $ fromMaybe 1 $ obj^?physics._Just.massInv
      translate = (+loc) . subtract floorHeight
      collides = filter ((<0) . snd)
               . map (id &&& (dot floorNormal . translate) . (tr r #>))
               $ vert
      minD :: Float
      minD = negate . foldr min 0 . map snd $ collides
      response (point, depth) =
        let
          vrel = dot floorNormal $ getVec speed obj + cross (getVec angularS obj) point
          aobj = dot floorNormal $ cross (i #> cross point floorNormal) point
          i    = getMat inertiaInv obj
          j    = negate (0.8) * vrel/(m+aobj)
          force = scale j floorNormal
          torque = getMat inertiaInv obj #> cross point force
            -- getMat inertiaInv obj #> cross point force
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

applyAcc :: GameObject -> GameObject
applyAcc = addScaled (physics._Just.speed) acc

applyForce :: GameObject -> GameObject
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
--          . floorReaction
          . applyAcc
--          . floatOnGround
          . set (physics._Just.angularS) newOm
          . set (physics._Just.inertiaInv) inInD
          . set (mesh.quaternion) newQ
          $ obj

applyLinear :: GameObject -> GameObject
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
          let maxspeed  = 1000000
              maxtorque = 100
              coeff1    = scale step . yolo maxspeed $ phy^.speed
              coeff2    = scale step . yolo maxtorque $ phy^.angularS
          in over (physics._Just.angularS) (<+> coeff2)
            . over (physics._Just.speed) (<+> coeff1)


  -- applyWithPhysics helper
  -- where helper phy obj =
  --         let quat   = obj^.mesh.quaternion
  --             rot    = rotateWithQuat quat [0,0,-1]
  --               -- (\(a,b,c) -> [a,b,c]) . imag . flip quatConcat (qInverse quat) $ quatConcat quat (0 +:: (0,0,-1))
  --             force  = 40 -- exp (maxspeed - sNorm) - 1
  --         in over (physics._Just.acc) (<+>cmap (*force) rot) obj
