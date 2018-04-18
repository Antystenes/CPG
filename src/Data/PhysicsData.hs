{-# LANGUAGE TemplateHaskell, OverloadedLists #-}

module Data.PhysicsData where


import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import           Control.Lens      hiding (indices)
import qualified Data.Vector.Storable as VS
import           Number.Quaternion (T)
import           Numeric.LinearAlgebra (Matrix, Vector,(><))
import qualified SDL
import qualified SDL.Image as SDLImage

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

startPhysics quat =
  let orientation = quatToMat3 quat
  in PhysicsData { _acc = [0,0,0]
                 , _speed = [0,0,0]
                 , _inertiaD = defaultInertiaDiag
                 , _inertiaInv = quatToLInv defaultInertiaDiag quat
                 , _angularS = [0,0,0]
                 , _torque = [0,0,0] }
