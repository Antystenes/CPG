{-# LANGUAGE OverloadedLists #-}

module OBJReader where

import qualified Data.HashMap as HM
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable ((!))
import           Control.Arrow
import           Control.Lens
import           Data.List (nub)
import qualified Graphics.Rendering.OpenGL.GL as GL


splitOn x =
  foldr (\c -> if c == x then ([]:) else over (ix 0) (c:)) [[]]

readOBJ :: FilePath -> IO (VS.Vector Float, VS.Vector GL.BaseInstance)
readOBJ fn = do
  text <- map words . lines <$> readFile fn
  let verts    = map (VS.fromList.map read.tail) . filter ((=="v").head) $ text
      normals  = map (VS.fromList.map read.tail) . filter ((=="vn").head) $ text
      faces    = (map ((\[a,_,b] -> (read a,read b)) . splitOn '/') . tail) =<< filter ((=="f").head) text
      faceNoDup= nub faces
      faceMap  = HM.fromList . flip zip [0..] $ faceNoDup
        --                 u v color       tangent bitangent
      addColor = (VS.++ [1,1,0.5,0.5,0.5,0,0,0,0,0,0])
      faceProc = subtract 1 *** subtract 1 >>> (verts!!) *** (normals!!) >>> uncurry (VS.++) >>> addColor
      vertData = VS.concat $ map faceProc faceNoDup
      indices  = VS.fromList . map (faceMap HM.!) $ faces
--      result   = VS.concat $ map faceProc faces
  return (vertData, indices)
