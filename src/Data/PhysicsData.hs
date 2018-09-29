{-# LANGUAGE TemplateHaskell, OverloadedLists, DeriveGeneric, DeriveAnyClass, Strict #-}

module Data.PhysicsData where

import           Control.Lens      hiding (indices)
import           Number.Quaternion (T)
import           Numeric.LinearAlgebra (Matrix, Vector,(><), scale)

import           Utils.Quaternions
import Control.DeepSeq
import GHC.Generics
import Control.Arrow

import Physics.Collision

data PhysicsData = PhysicsData {
  _acc      :: Vector Float,
  _speed    :: Vector Float,
  _inertiaD :: Matrix Float,
  _massInv  :: Float,
  _inertiaInv :: Matrix Float,
  _angularS :: Vector Float,
  _torque   :: Vector Float } deriving Show

makeLenses ''PhysicsData

defaultInertiaDiag :: Float -> Matrix Float
defaultInertiaDiag m =
  (3><3)[ m, 0, 0
        , 0, m, 0
        , 0, 0, m ]


setRectInertia x y z m = set inertiaD . scale (m/12) $
  (3><3) [(x^2+z^2),         0,         0
         ,        0, (x^2+z^2),         0
         ,        0,         0, (x^2+y^2)]

carPhysics =
  setRectInertia 1 1 10 &&& startPhysics >>> uncurry (.)

startPhysics :: Float -> T Float -> PhysicsData
startPhysics mInv quat =
  PhysicsData { _acc        = [0,0,0]
              , _speed      = [0,0,0]
              , _inertiaD   = defaultInertiaDiag (1/mInv)
              , _massInv    = mInv
              , _inertiaInv = quatToLInv (defaultInertiaDiag (1/mInv)) quat
              , _angularS   = [0,0,0]
              , _torque     = [0,0,0] }
