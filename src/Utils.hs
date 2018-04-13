{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import           Language.Haskell.TH.Syntax
import           Data.Proxy
import qualified Graphics.GL as GLRaw
import           GHC.TypeLits
import qualified Graphics.Rendering.OpenGL.GL as GL


glTexture :: GL.GLuint -> GLRaw.GLenum
glTexture = (GLRaw.GL_TEXTURE0+) . fromIntegral

-- glTexture :: Int -> Q Name
-- glTexture n =
--   let number = show n
--       pref   = showName 'GLRaw.GL_TEXTURE
--   in return . mkName $ pref ++ number


-- glTexture :: Int -> GLRaw.GLenum
-- glTexture n = $(genGlText n)
