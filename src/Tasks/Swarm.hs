{-# LANGUAGE OverloadedLists #-}
module Tasks.Swarm where

import           Numeric.LinearAlgebra (Vector, flatten, scale)
import           Control.Lens

import Data.GameObject
import Data.PhysicsData
import Physics.Mechanics

import Utils (norm, step, normalize)

addGravityForce :: Vector Float -> GameObject -> GameObject
addGravityForce gravCent obj =
  let direction = gravCent - obj^.location
      dnorm = norm direction
      force = -- scale (1/dnorm)
              -- normalize $
              direction
  in putForceOnTip force obj
    -- over (physics._Just.acc) (+force) obj

objectNeighbours :: GameObject -> [GameObject] -> [GameObject]
objectNeighbours obj objs =
  let loc = obj^.location
      maxDist = 3
  in filter ((maxDist >) . norm . (loc -) . view location) objs

separationsForce :: GameObject -> [GameObject] -> Vector Float
separationsForce obj =
  let loc = obj^.location
      minDist = 1 :: Float
      checkVec v =
        let n = norm v
        in if minDist > n && n > 0.000001
           then scale 10 v :: Vector Float
           else 0
  in
    foldr ((+).checkVec) [0,0,0]
    . map ((loc-) . view location)

addSeparationForce :: [GameObject] -> GameObject -> GameObject
addSeparationForce neighb obj =
  let sepForce = separationsForce obj neighb
  in putForceOnTip sepForce obj
    -- over (physics._Just.acc) (+sepForce) obj
