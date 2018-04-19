{-# LANGUAGE TemplateHaskell, OverloadedLists #-}

module Data.PhysicsData where


import           Control.Lens      hiding (indices)
import           Number.Quaternion (T)
import           Numeric.LinearAlgebra (Matrix, Vector,(><))

import           Utils.Quaternions

data PhysicsData = PhysicsData {
  _acc      :: Vector Float,
  _speed    :: Vector Float,
  _inertiaD :: Matrix Float,
  _inertiaInv :: Matrix Float,
  _angularS :: Vector Float,
  _torque   :: Vector Float } deriving Show

makeLenses ''PhysicsData

defaultInertiaDiag :: Matrix Float
defaultInertiaDiag =
  (3><3)[ 1, 0,   0
        , 0, 0.5, 0
        , 0, 0,   3 ]

startPhysics :: T Float -> PhysicsData
startPhysics quat =
  PhysicsData { _acc = [0,0,0]
              , _speed = [0,0,0]
              , _inertiaD = defaultInertiaDiag
              , _inertiaInv = quatToLInv defaultInertiaDiag quat
              , _angularS = [0,0,0]
              , _torque = [0,0,0] }
