{-# LANGUAGE OverloadedLists #-}

module Utils.Quaternions where

import           Number.Quaternion hiding (normalize,norm)
import qualified Number.Quaternion as Q
import           Numeric.LinearAlgebra (Matrix, Vector, tr, cmap,(<>))
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra.Data as LD

import Utils (mat3ToMat4)

qvMul :: VS.Vector Float -> T Float -> T Float
qvMul [b,c,d] = quatConcat (0 +:: (b,c,d))
  -- let w      = real quat
  --     (x,y,z)= imag quat
  -- in w +:: (x*b,c*y,d*z)

quatToLInv inertiaDiag quat =
  let orientation = quatToMat3 quat
  in tr orientation
   <> cmap (\x -> if x == 0 then 0 else 1/x) inertiaDiag
   <> orientation

rotateWithQuat :: T Float -> VS.Vector Float -> VS.Vector Float
rotateWithQuat quat [a,b,c] = (\(a,b,c) -> [a,b,c]) . imag . flip quatConcat (qInverse quat) $ quatConcat quat (0 +:: (a,b,c))

addQuat q1 q2 =
  let r1 = real q1
      r2 = real q2
      i1 = imag q1
      i2 = imag q2
  in (r1 + r2) +:: sum3 i1 i2


quatToMat = mat3ToMat4 . quatToMat3

quatToMat3 :: T Float -> Matrix Float
quatToMat3 = LD.fromArray2D . toRotationMatrix

sum3 (a,b,c) (d,e,f) = (a+d,b+e,c+f)

scale3 n (a,b,c) = (n*a,n*b,n*c)

quatConcat q0 q1 =
  let w0 = real q0
      w1 = real q1
      v0 = imag q0
      v1 = imag q1
      wr = w1 * w0 - scalarProduct v1 v0
      vr = scale3 w1 v0 `sum3` scale3 w0 v1 `sum3` crossProduct v1 v0
  in wr +:: vr

qInverse q =
  let w = real q
      v = (\(a,b,c) -> (negate a, negate b, negate c)) $ imag q
  in w +:: v
