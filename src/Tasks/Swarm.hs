{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Tasks.Swarm where

import           Numeric.LinearAlgebra (Vector, scale)
import qualified Data.Vector.Storable as VS
import           Control.Lens
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Vector.Mutable (STVector)
import           Control.Monad.ST
import           Data.List (nub)
import           Control.Monad ((<=<), foldM, when)
import Debug.Trace
import qualified Data.HashTable.ST.Cuckoo as HT
import qualified Data.HashMap as HM
import Data.Vector.Strategies
import GHC.Generics
import Control.Arrow

import Data.GameObject
import Physics.Mechanics
import Physics.Collision
import Data.PhysicsData

import Utils (norm,normalize)

type CollisionGrid s = HT.HashTable s (Int,Int,Int) [Int]

integrityWeight = 0.5
separationWeight = 2
gravityWeight = 0.1

cellSize = 4

vehSize = 2
minDist = 4

off = map ((.) (round.(/cellSize))) [(+ vehSize),(+ negate vehSize)]

addInPl :: forall s. Vector Float -> STVector s Float -> ST s ()
addInPl v mv = VS.zipWithM_ (\i a -> MV.modify mv (+a) i) [0,1,2] v

data BoidComputation = BoidComputation { _object :: GameObject
                                       , _sumSp :: Vector Float
                                       , _separ :: Vector Float
                                       , _nrNeighb :: {-# UNPACK #-} Int} deriving Show

makeLenses ''BoidComputation

ins :: Int -> Maybe [Int] -> (Maybe [Int], ())
ins ind (Just l) = (Just (ind:l),())
ins ind Nothing  = (Just [ind],())

markObject t ind=
  mapM_ (\v -> HT.mutate t (VS.unsafeIndex v 0,VS.unsafeIndex v 1,VS.unsafeIndex v 2) $ ins ind) .
  VS.mapM (\x -> ($ x) <$> off) .
  view location

createGrid :: V.Vector GameObject ->
               ST s (CollisionGrid s, V.Vector PrimitiveComputation)
createGrid v = do
  ht <- HT.newSized 6000
  vec <- V.imapM (process ht) v
  return (ht, vec)
  where
    process ht ind ob = do
      markObject ht ind ob
      return $ prepareCPrim ob

getCollisionData :: CollisionGrid s ->
                     V.Vector PrimitiveComputation ->
                     ST s (HM.Map (Int, Int) [Contact])
getCollisionData g prims =
  HT.foldM (\a p -> return $ collisionsInCell prims a (second nub p)) HM.empty g

getContacts :: V.Vector GameObject -> HM.Map (Int, Int) [Contact]
getContacts v =
  traceShowId $ runST $ createGrid v >>= uncurry getCollisionData

resolveContacts :: V.Vector GameObject -> V.Vector GameObject
resolveContacts =
  getContacts >>= HM.foldWithKey folder id
  where folder k = (.) . resolveCollision k


collisionsInCell :: V.Vector PrimitiveComputation ->
                      HM.Map (Int, Int) [Contact] ->
                      (a, [Int]) ->
                      HM.Map (Int, Int) [Contact]
collisionsInCell prims m (x, ix1:ix2:l) =
  let newM = insertCollisions prims m ix1 ix2
  in collisionsInCell prims newM (x, ix2:l)
collisionsInCell _ m _ = m

insertCollisions :: V.Vector PrimitiveComputation ->
                      HM.Map (Int, Int) [Contact] ->
                      Int -> Int ->
                      HM.Map (Int, Int) [Contact]
insertCollisions prims m ix1 ix2 =
  let key = (ix1, ix2)
      check = V.unsafeIndex prims
  in if HM.member key m
     then m
     else let val = view contacts $
                    detectCollision (check ix1) (check ix2)
          in if null val
             then m
             else HM.insert key val m



fillGrid :: V.Vector GameObject -> ST s (CollisionGrid s, STVector s BoidComputation)
fillGrid v = do
  ht <- HT.newSized 6000
  vec <- V.thaw =<< V.imapM (process ht) v
  return (ht,vec)
  where
    process g ind ob = do
      markObject g ind ob
      return $ initBoidComputation ob

computeForces :: (GameObject -> GameObject) -> V.Vector GameObject -> V.Vector GameObject
computeForces = computeForcesGrid fillGrid

incCount = over nrNeighb (+1)

initBoidComputation v = BoidComputation v [0,0,0] [0,0,0] 0

computeBoid (BoidComputation o v1 v2 n) =
  if n /= 0 then
    let Just p = o^.physics
        integ  = scale (integrityWeight / fromIntegral n) $ v1 - p^.speed
        sep    = scale separationWeight v2
    in putForceOnMid (integ+sep) o
  else o

computeForcesGrid :: (forall s. V.Vector GameObject -> ST s (CollisionGrid s, STVector s BoidComputation)) -> (GameObject -> GameObject) -> V.Vector GameObject -> V.Vector GameObject
computeForcesGrid s f v =
  V.map (f . computeBoid) $ runST $
             V.freeze <=< uncurry computeForcesMap <=< s $ v

computeForcesMap :: CollisionGrid s -> STVector s BoidComputation -> ST s (STVector s BoidComputation)
computeForcesMap m v = HT.foldM (\x -> computeForcesST x . nub . snd) v m

computeForcesST :: STVector s BoidComputation -> [Int] -> ST s (STVector s BoidComputation)
computeForcesST v (i:l@(_:_)) =
  foldM (computeForcesPair i) v l >>= flip computeForcesST l
computeForcesST v _ = pure v

computeForcesPair
  :: Int
  -> STVector s BoidComputation
  -> Int
  -> ST s (STVector s BoidComputation)
computeForcesPair i1 v i2 = do
  obj1 <- view object <$> MV.read v i1
  obj2 <- view object <$> MV.read v i2
  when (norm (obj1^.location - obj2^.location) < minDist) $ do
    let sepForce = sep obj1 obj2
    MV.modify v (over separ (+ sepForce)) i1
    MV.modify v (over sumSp (+ integ obj2)) i1
    MV.modify v (over separ (subtract sepForce)) i2
    MV.modify v (over sumSp (+ integ obj1)) i2
    MV.modify v incCount i1
    MV.modify v incCount i2
  pure v

sep :: GameObject -> GameObject -> Vector Float
sep obj1 obj2 = obj1^.location - obj2^.location
  -- let v = obj1^.location - obj2^.location
  --     m = norm v
  -- in scale (1/m/(m-1)) v

integ :: GameObject -> Vector Float
integ obj =
  case obj^.physics of
    Just x  -> x^.speed
    Nothing -> [0,0,0]

clamp :: Float -> Vector Float -> Vector Float
clamp m v =
  let n = norm v
  in if n > m then scale (m/n) v else v

addGravityForce :: Vector Float -> GameObject -> GameObject
addGravityForce gravCent obj =
  let direction = gravCent - obj^.location
--       dnorm = norm direction
      force = scale gravityWeight
              -- . normalize $
              direction
  in putForceOnMid force obj
    -- over (physics._Just.acc) (+force) obj
