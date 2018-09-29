{-# LANGUAGE TemplateHaskell
           , TupleSections
           , DeriveGeneric
           , DeriveAnyClass
           , OverloadedLists #-}
module Physics.Collision where

import Numeric.LinearAlgebra (Vector, Matrix, (<>), scale, dot, cross, (#>))
import Control.Lens
import Numeric.LinearAlgebra.Data
import Data.Vector.Storable (init)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Function (on)

import Control.DeepSeq
import Utils
import Prelude hiding (init)

data Contact = Contact { _contactPoint :: Vector Float
                       , _contactNormal :: Vector Float
                       , _penetration :: Float
                       , _sign :: Float } deriving Show

makeLenses ''Contact

makeContactBasis cont =
  let ys = [0,1,0]
      y2 = [0,0,1]
      x  = cont^.contactNormal
      zs = normalize $ cross x ys
      z  = if VS.any isNaN zs
           then normalize $ cross x y2
           else zs
      y  = normalize $ cross z x
  in fromRows [x,y,z]

data CollisionData = CollisionData { _contacts :: [Contact]
                                   , _left :: Int }

makeLenses ''CollisionData

data Primitive = Sphere { _radius     :: Float
                        , _primoffset :: Matrix Float }
               | Plane  { _normal     :: Vector Float
                        , _planeoff   :: Float }
               | Box    { _points     :: V.Vector (Vector Float)
                        , _faces      :: V.Vector Squad }
                 deriving Show

data PrimitiveComputation = CSphere { _r    :: Float
                                    , _poff :: Vector Float }
                          | CPlane  { _cnormal   :: Vector Float
                                    , _cplaneoff :: Float}
                          | CBox    { _cpoints :: V.Vector (Vector Float)
                                    , _quads :: V.Vector Quad } deriving Show

data Quad = Quad { _qnorm   :: Vector Float
                 , _qoff    :: Float} deriving Show

quadToCPlane (Quad n o) = CPlane n o

data Squad = Squad { _snorm :: Vector Float
                   , _spoint:: Vector Float } deriving Show

sToQ t (Squad n p) = Quad newN o
  where
    newN = init . (t#>) . flip VS.snoc 0 $ n
    o    = dot newN . init . (t#>) . flip VS.snoc 1 $ p

preparePrimitive :: Matrix Float -> Primitive -> PrimitiveComputation
preparePrimitive t (Sphere r p1) = CSphere r ppos
  where ppos = init . (!!3) . toColumns $ t <> p1
preparePrimitive t (Box p q) = CBox (V.map adjust p) $ V.map (sToQ t) q
  where adjust = init . (t #>) . flip VS.snoc 1
preparePrimitive _ (Plane n o) = CPlane n o

tolerance = 0.05

quadDepth (Quad n o) p =
  (dot n p - o, n)

boxPtcoll (CBox _ qs) p cdata =
  let dots = V.map (flip quadDepth p) qs
      (d,n) = V.minimumBy (compare `on` fst) dots
  in if V.any ((>=0) . fst) dots
     then cdata
     else over contacts (Contact p n d 1:) cdata

quadPenetration (Quad n o) = planePenetration (CPlane n o)

planePenetration (CPlane n o) p cdata =
  let vertDist = dot p n
      cPoint   = p -- scale (vertDist - o) n + p
      contact  = Contact cPoint n (o - vertDist) 1
  in if cdata^.left > 0 && vertDist <= o + tolerance
     then over contacts (contact:) . over left (subtract 1) $ cdata
     else cdata

-- checkAxe :: Vector Float -> Vector Float -> V.Vector
checkAxe pos r ps ix =
  let vals = V.map (VS.!ix) ps
  in (pos VS.! ix + r) >= minimum vals &&
     (pos VS.! ix - r) <= maximum vals

checkConvexAxe ps1 ps2 ix =
  let vals1 = V.map (VS.! ix) ps1
      vals2 = V.map (VS.! ix) ps2
  in maximum vals1 >= minimum vals2 &&
     minimum vals1 <= maximum vals2

axeList :: [Int]
axeList = [0,1,2]

separatingAxes :: PrimitiveComputation -> PrimitiveComputation -> Bool
separatingAxes (CSphere r pos) (CBox ps _) = all (checkAxe pos r ps)     axeList
separatingAxes (CBox ps _) (CSphere r pos) = all (checkAxe pos r ps)     axeList
separatingAxes (CBox ps _) (CBox ps2 _)    = all (checkConvexAxe ps ps2) axeList

detectCollision prim1 prim2 =
  collisionDetector prim1 prim2 (CollisionData [] 8)

quadSphereCollision (CSphere r p) q@(Quad n o) =
  (dot n p - r - o, n)

quadContact (CSphere r p) (dist, n) =
  let contactPos = p - scale (dist + r) n
  in Contact contactPos n (-dist) 1

collisionDetector (CSphere r1 pos1) (CSphere r2 pos2) cdata =
  let mid     = pos1 - pos2
      midnorm = norm mid
  in if cdata^.left <= 0 || midnorm <= 0.0 || midnorm >= r1 + r2
     then cdata
     else let normal     = scale (1/midnorm) mid
              contactPos = pos1 + scale 0.5 mid
              contactPen = r1 + r2 - midnorm
              contact    = Contact contactPos normal contactPen 1
          in over contacts (contact:) . over left (subtract 1) $ cdata
collisionDetector p@(CPlane _ _) s@(CSphere _ _) cdata =
  set (contacts.traverse.sign) (-1) $
  collisionDetector s p cdata
collisionDetector (CSphere r pos1) (CPlane n p2) cdata =
  if cdata^.left <= 0 then cdata
  else let dist = dot n pos1 - r - p2
       in if dist >= 0
          then cdata
          else let contactPos = pos1 - scale (dist + r) n
                   contact    = Contact contactPos n (-dist) 1
               in over contacts (contact:) cdata
collisionDetector (CBox ps _) p@(CPlane _ _) cdata =
  V.foldr (planePenetration p) cdata ps
collisionDetector s@(CSphere _ _) b@(CBox _ _) cdata =
  set (contacts.traverse.sign) (-1) $
  collisionDetector b s cdata
collisionDetector (CBox _ q) s@(CSphere _ _) cdata =
  let cols = V.map (quadSphereCollision s) q
      best = V.maximumBy (compare `on` fst) cols
  in if V.any ((>=0) . fst) cols
     then cdata
     else over contacts (quadContact s best:) cdata
collisionDetector p@(CPlane _ _) b@(CBox _ _) cdata =
  set (contacts.traverse.sign) (-1) $
  collisionDetector b p cdata
collisionDetector b1@(CBox ps qs) b2@(CBox ps2 qs2) cdata =
  V.foldr (boxPtcoll b2) (V.foldr (boxPtcoll b1) cdata ps2) ps
collisionDetector _ _ cdata = cdata
