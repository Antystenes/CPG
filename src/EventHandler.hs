{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
module EventHandler where

import qualified SDL
import           Control.Lens


import Data.Scene
import Physics.Mechanics

handleQuit :: SDL.Event -> Bool
handleQuit event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEv ->
    SDL.keyboardEventKeyMotion keyEv == SDL.Pressed &&
    SDL.keysymKeycode (SDL.keyboardEventKeysym keyEv) == SDL.KeycodeEscape
  _ -> False

keyHandler :: SDL.Scancode -> Scene -> Scene
keyHandler SDL.ScancodeA = -- moveLeftCamera
  over (objects.player) $ putForceOnSideLoc [0,-2,0]
keyHandler SDL.ScancodeD = -- moveRightCamera
  over (objects.player) $ putForceOnSideLoc [0,2,0]
keyHandler SDL.ScancodeW = moveForwardCamera
keyHandler SDL.ScancodeS = moveBackCamera
keyHandler SDL.ScancodeLeft = over (objects.player) $ putForceOnTipLoc [10,0,0] -- turnQuatZ (step * 2) --   -- $
keyHandler SDL.ScancodeRight = over (objects.player) $ putForceOnTipLoc [-10,0,0]  -- turnQuatZ (negate step * 2) --  -- $
keyHandler SDL.ScancodeDown = over (objects.player) $ putForceOnTipLoc [0,10,0] -- turnQuatZ (step * 2) --   -- $
keyHandler SDL.ScancodeUp = over (objects.player) $ putForceOnTipLoc [0,-10,0]  -- turnQuatZ (negate step * 2) --  -- $
keyHandler SDL.ScancodeSpace = over (objects.player) boost
keyHandler _             = id

keys :: [SDL.Scancode]
keys = [SDL.ScancodeA,
        SDL.ScancodeD,
        SDL.ScancodeW,
        SDL.ScancodeS,
        SDL.ScancodeSpace,
        SDL.ScancodeLeft,
        SDL.ScancodeRight,
        SDL.ScancodeDown,
        SDL.ScancodeUp]
