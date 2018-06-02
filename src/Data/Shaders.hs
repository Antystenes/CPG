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

loadShaders :: String -> String -> [String] -> IO Shaders
loadShaders vertNm fragNm uniformNms = do
  vertexShader   <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader
  vertSrc        <- BS.readFile vertNm
  fragSrc        <- BS.readFile fragNm
  GL.shaderSourceBS vertexShader $= vertSrc
  GL.shaderSourceBS fragmentShader $= fragSrc
  GL.compileShader vertexShader
  GL.compileShader fragmentShader
  prog <- GL.createProgram
  GL.attachShader prog vertexShader
  GL.attachShader prog fragmentShader
  GL.linkProgram prog
  GL.detachShader prog vertexShader
  GL.detachShader prog fragmentShader
  Shaders prog <$> foldM (addUniform prog) HM.empty uniformNms
    where
      addUniform prog hm name = do
        GL.UniformLocation ident <- GL.uniformLocation prog name
        return $ HM.insert name ident hm

uniformName shaders name =
  HM.findWithDefault (-1) name $ shaders^.uniforms
