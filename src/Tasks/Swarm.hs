module Tasks.Swarm where

import           Numeric.LinearAlgebra (Vector, flatten, scale)
import           Control.Lens

import Data.GameObject
import Data.PhysicsData

import Utils (norm)


addGravityForce :: Vector Float -> GameObject -> GameObject
addGravityForce gravCent obj =
  let direction = gravCent - obj^.location
      dnorm = norm direction
      force = scale (10/dnorm) direction
  in over (physics._Just.acc) (+force) obj


objectNeighbours obj objs =
  let loc = obj^.location
      maxDist = 3
  in filter ((maxDist >) . norm . (loc -) . view location) objs

separationsForce obj neighb =
  let loc = obj^.location
      minDist = 1 :: Float
      checkVec v =
        let n = norm v
        in if minDist > n && n > 0.000001
           then scale 1 v :: Vector Float
           else 0
  in
    foldr ((+).checkVec) 0
    . map ((loc-) . view location)
    $ neighb

addSeparationForce neighb obj =
  let sepForce = separationsForce obj neighb
  in over (physics._Just.acc) (+sepForce) obj
