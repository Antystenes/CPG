{-# LANGUAGE MagicHash
           , UnboxedTuples
           , DataKinds
           , TypeFamilies #-}

module Internal.Vector where

import GHC.Exts
import GHC.TypeLits

data family Vector (n :: Nat)

data family MVector (n :: Nat)

data instance Vector 3 = Vector3 (# Int#, Int#, Int# #)
