{-# LANGUAGE TemplateHaskell #-}

module Data.Shaders where

import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Data.ByteString              as BS
import qualified Data.HashMap                 as HM
import           Control.Lens      hiding (indices)
import           Control.Monad (foldM)

data Shaders = Shaders { _program  :: GL.Program,
                         _uniforms :: HM.Map String GL.GLint }

makeLenses ''Shaders

loadAndCompileShader stype name = do
  shad <- GL.createShader stype
  shadSrc <- BS.readFile name
  GL.shaderSourceBS shad $= shadSrc
  GL.compileShader shad
  return shad

loadShaders :: String -> String -> [String] -> IO Shaders
loadShaders vertNm fragNm uniformNms = do
  vertShader <- loadAndCompileShader GL.VertexShader vertNm
  fragShader <- loadAndCompileShader GL.FragmentShader fragNm
  prog <- GL.createProgram
  mapM_ (GL.attachShader prog) [vertShader, fragShader]
  GL.linkProgram prog
  mapM_ (GL.detachShader prog) [vertShader, fragShader]
  Shaders prog <$> foldM (addUniform prog) HM.empty uniformNms
    where
      addUniform prog hm name = do
        GL.UniformLocation ident <- GL.uniformLocation prog name
        return $ HM.insert name ident hm

uniformName shaders name =
  HM.findWithDefault (-1) name $ shaders^.uniforms
