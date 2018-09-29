{-# LANGUAGE OverloadedLists
           , TypeFamilies
           , FlexibleContexts #-}

module Utils.Quaternions where

import           Number.Quaternion hiding (normalize,norm)
import qualified Number.Quaternion as Q
import           Numeric.LinearAlgebra (Matrix, Vector, tr, cmap,(<>), inv,single,double)
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra.Data as LD

import Utils (mat3ToMat4)

qvMul :: VS.Vector Float -> T Float -> T Float
qvMul v = quatConcat (0 +:: (VS.unsafeIndex v 0,VS.unsafeIndex v 1,VS.unsafeIndex v 2))

quatToLInv inertiaDiag quat =
  let orientation = quatToMat3 quat
  in tr orientation
   <> (single . inv . double) inertiaDiag
   <> orientation

rotateWithQuat :: T Float -> VS.Vector Float -> VS.Vector Float
rotateWithQuat quat [a,b,c] =
  (\(a,b,c) -> [a,b,c]) .
  imag .
  flip quatConcat (qInverse quat) $
  quatConcat quat (0 +:: (a,b,c))

addQuat q1 q2 =
  let r1 = real q1
      r2 = real q2
      i1 = imag q1
      i2 = imag q2
  in (r1 + r2) +:: sum3 i1 i2


quatToMatV :: T Float -> VS.Vector Float
quatToMatV q =
  let r = real q
      (i,j,k) = imag q
      r2 = r*r
      i2 = i*i;   j2 = j*j;   k2 = k*k;
      ri = 2*r*i; rj = 2*r*j; rk = 2*r*k;
      ij = 2*i*j; ki = 2*k*i; kj = 2*k*j;
  in [ r2+i2-j2-k2,       ij-rk,       ki+rj, 0
     ,       ij+rk, r2-i2+j2-k2,       kj-ri, 0
     ,       ki-rj,       kj+ri, r2-i2-j2+k2, 0
     ,           0,           0,           0, 1]

quatToMat = tr . LD.reshape 4 . quatToMatV

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

qInverse :: T Float -> T Float
qInverse q =
  let w = real q
      v = (\(a,b,c) -> (negate a, negate b, negate c)) $ imag q
  in w +:: v

qZero :: T Float
qZero = 1 +:: (0,0,0)
