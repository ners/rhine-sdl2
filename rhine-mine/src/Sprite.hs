{-# LANGUAGE TemplateHaskell #-}

module Sprite where

import Data.FileEmbed (embedFileRelative)
import FRP.Rhine
import Foreign.C (CInt)
import SDL qualified
import SDL.Image qualified as SDL
import Prelude

getSprite :: (MonadIO m) => m SDL.Surface
getSprite = SDL.decode $(embedFileRelative "assets/sprite.png")

tileSize :: (Integral a) => a
tileSize = 16

tileRect :: (Integral a) => Int -> Int -> SDL.Rectangle a
tileRect y x =
    SDL.Rectangle
        (SDL.P $ SDL.V2 (fromIntegral x * tileSize) (fromIntegral y * tileSize))
        (SDL.V2 tileSize tileSize)

data Sprite
    = Uncovered Int
    | Covered
    | Bomb
    | Flag

spriteRect :: (Integral a) => Sprite -> SDL.Rectangle a
spriteRect (Uncovered i)
    | i >= 1 && i < 5 = tileRect 0 (i - 1)
    | i >= 5 && i < 9 = tileRect 1 (i - 5)
    | i == 0 = tileRect 2 0
spriteRect Covered = tileRect 2 1
spriteRect Bomb = tileRect 2 2
spriteRect Flag = tileRect 2 3
spriteRect _ = undefined

drawSprite
    :: (MonadIO m)
    => SDL.Surface
    -> Sprite
    -> SDL.Surface
    -> SDL.Rectangle CInt
    -> m ()
drawSprite src sprite dst dstPos = SDL.surfaceBlitScaled src (Just $ spriteRect sprite) dst (Just dstPos)
