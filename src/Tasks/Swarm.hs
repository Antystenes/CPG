{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes#-}
module Tasks.Swarm where

import           Numeric.LinearAlgebra (Vector, scale)
import qualified Data.Vector.Storable as VS
import           Control.Lens
import           Data.Maybe (isJust,maybe)
import qualified Data.HashMap as HM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Vector.Mutable (STVector)
import           Control.Monad.ST
import           Data.List (nub)
import           Control.Monad ((<=<), foldM, when)
import Debug.Trace
import qualified Data.HashTable.ST.Basic as HT


import Data.GameObject
import Physics.Mechanics
import Data.PhysicsData

import Utils (norm,normalize)

type CollisionGrid s = HT.HashTable s (Int,Int,Int) [Int]

integrityWeight = 0.4
separationWeight = 0.4
gravityWeight = 0.4

cellSize = 4

vehSize = 0.7
minDist = 3

off = map ((.) (round.(/cellSize))) [(+ vehSize),(+ negate vehSize)]


data BoidComputation = BoidComputation { _object :: !GameObject
                                       , _sumSp :: !(Vector Float)
                                       , _separ :: !(Vector Float)
                                       , _nrNeighb :: !Int} deriving Show

makeLenses ''BoidComputation

fillGrid :: V.Vector GameObject -> ST s (CollisionGrid s, STVector s BoidComputation)
fillGrid v = do
  ht <- HT.newSized 400
  v <- V.imapM (process ht) v >>= V.thaw
  return (ht,v)
  where
    markObject ind obj t =
      let loc = obj^.location
      in mapM_ (\v -> HT.mutate t (VS.unsafeIndex v 0,VS.unsafeIndex v 1,VS.unsafeIndex v 2) (ins ind))
       . VS.mapM (\x -> ($ x) <$> off)
       $ loc

    ins :: Int -> Maybe [Int] -> (Maybe [Int], ())
    ins ind (Just l) = (Just (ind:l),())
    ins ind Nothing  = (Just [ind],())

    process :: CollisionGrid s -> Int -> GameObject -> ST s BoidComputation
    process g ind ob = do
      markObject ind ob g
      return $ initBoidComputation ob

computeForces :: V.Vector GameObject -> V.Vector GameObject
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

computeForcesGrid :: (forall s. V.Vector GameObject -> ST s (CollisionGrid s, STVector s BoidComputation)) -> V.Vector GameObject -> V.Vector GameObject
computeForcesGrid s v =
  V.map computeBoid $ runST $
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
    MV.modify v (over separ (subtract sepForce)) i2
    MV.modify v (over sumSp (+ integ obj2)) i1
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

addGravityForce :: Vector Float -> GameObject -> GameObject
addGravityForce gravCent obj =
  let direction = gravCent - obj^.location
--       dnorm = norm direction
      force = scale gravityWeight
              -- . normalize $
              direction
  in putForceOnMid force obj
    -- over (physics._Just.acc) (+force) obj
