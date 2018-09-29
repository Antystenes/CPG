{-# LANGUAGE OverloadedLists #-}

module OBJReader where

import qualified Data.HashMap as HM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import           Data.Vector.Storable ((!))
import           Control.Arrow
import           Control.Lens
import           Data.List (nub, nubBy)
import qualified Numeric.LinearAlgebra as L
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Data.Function (on)
import Debug.Trace

import Physics.Collision

splitOn x =
  foldr (\c -> if c == x then ([]:) else over (ix 0) (c:)) [[]]

addP (a,b) (c,d) = (a+b,c+d)

getFace :: [String] -> [(Int, Int)]
getFace =
  map ((\[v,_,n] -> (read v - 1,read n - 1)) . splitOn '/') . tail

readVectors :: String -> [[String]] -> [VS.Vector Float]
readVectors vtype =
  map (VS.fromList.map read.tail) . filter ((==vtype) . head)

readPoints = readVectors "v"
readNormals = readVectors "vn"

readBoxFromObj :: FilePath -> IO Primitive
readBoxFromObj fn = do
  text <- map words . lines <$> readFile fn
  let p = V.fromList . readPoints $ text
      q = V.fromList . readNormals $  text
      f = map (V.unsafeIndex p *** V.unsafeIndex q >>> uncurry (flip Squad)) .
          nubBy ((==) `on` snd) .
          map (head . getFace) .
          filter ((=="f").head) $ text
  return $ Box p (V.fromList f)

readOBJ :: FilePath -> IO (VS.Vector Float, VS.Vector GL.BaseInstance,[VS.Vector Float])
readOBJ fn = do
  text <- map words . lines <$> readFile fn
  let
    verts    = readPoints text
    normals :: [VS.Vector Float]
    normals  = readNormals text
    faces    = map getFace . filter ((=="f").head) $ text
    vertData = nub . concat $ faces
    vertIxMap= HM.fromList . flip zip [0..] $ vertData
    --                 u v color
    addColor = (VS.++ [1,1,0.5,0.5,0.5])
    computeT :: [(Int,Int)] -> VS.Vector Float
    computeT [v0, v1, v2] =
      let dpos1 = verts !! fst v1 - verts !! fst v0
          -- dpos2 = verts !! fst v2 - verts !! fst v0
          tang  = dpos1
          -- bitan = dpos2
      in tang
    nullTans = HM.fromList . flip zip (repeat $ VS.replicate 3 0) $ vertData
    insertT l h = let p = computeT l
                  in foldr (\k -> HM.insertWith (+) k p) h l
    tangents = foldr insertT nullTans faces
    normalize :: (Int, Int) -> VS.Vector Float -> VS.Vector Float
    normalize (_,k) t =
      let n                 = normals !! k
          normalizedTangent = normalizeVec $ t - L.scale (L.dot n t) n
          normalizedBitangent = L.cross normalizedTangent n
      in -- n VS.++ t
        normalizedTangent VS.++ normalizedBitangent
    normalizedTangents = HM.fromList . map (fst &&& uncurry normalize) . HM.toList $ tangents
    faceProc p = (verts!!) *** (normals!!) >>> uncurry (VS.++) >>> addColor >>> flip (VS.++) (normalizedTangents HM.! p) $ p
    vBuffData = VS.concat $ map faceProc vertData
    vertices  = map ((verts!!).fst) vertData
    indices  = VS.fromList . map (vertIxMap HM.!) . concat $ faces
--      result   = VS.concat $ map faceProc faces
  return (vBuffData, indices, vertices)

normalizeVec :: VS.Vector Float -> VS.Vector Float
normalizeVec v = L.scale (realToFrac . (1/) $ L.norm_2 v) v


-- compute
