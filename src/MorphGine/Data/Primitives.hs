{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , OverloadedLists
           , ScopedTypeVariables
           , TypeApplications #-}

module MorphGine.Data.Primitives where

import qualified Data.Vector.Storable  as VS
import qualified Numeric.LinearAlgebra as L
import           Numeric.LinearAlgebra ((><))
import qualified Numeric.LinearAlgebra.Data as DL --(flatten, reshape,cmap)
import           GHC.Ptr (Ptr(..))
import           GHC.TypeLits
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.GL as GLRaw
import           Graphics.GL (GLfloat)

data Matrix :: * -> Nat -> Nat -> * where
  M :: VS.Storable a => L.Matrix a -> Matrix a m n

withPtr :: (VS.Storable a, L.Element a) => Matrix a m n -> (Ptr a -> IO b) -> IO b
withPtr (M x) = VS.unsafeWith . L.flatten $ x

projection :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix GLfloat 4 4
projection fov aspect near far = M . (4><4) $ [ t/aspect, 0, 0, 0
                                               , 0,        t, 0, 0
                                               , 0,        0, (far+near)/(near-far), (2*far*near)/(near-far)
                                               , 0,        0,-1, 0]
  where t = 1 / tan (fov/2)

degProjection :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix  GLfloat 4 4
degProjection = projection . (/180) . (*pi)
