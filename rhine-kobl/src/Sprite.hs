{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Sprite where

import Font (FontCache)
import SDL qualified
import SDL.Font qualified
import SDL.Font qualified as SDL (Font)
import SDL.Image qualified as SDL
import Text
import Prelude

-- | A sprite sheet with rectangular images
class RectSprite a where
    tileSize :: CInt
    spriteRect :: a -> SDL.Rectangle CInt

drawRectSprite
    :: (MonadIO m, RectSprite sprite)
    => SDL.Renderer
    -> SDL.Texture
    -> sprite
    -> SDL.Rectangle CInt
    -> CDouble
    -> Maybe (SDL.Point SDL.V2 CInt)
    -> SDL.V2 Bool
    -> m ()
drawRectSprite renderer texture sprite dstRect = SDL.copyEx renderer texture (Just $ spriteRect sprite) (Just dstRect)

drawRectSprite'
    :: (MonadIO m, RectSprite sprite)
    => SDL.Renderer
    -> SDL.Texture
    -> sprite
    -> SDL.Rectangle CInt
    -> CDouble
    -> Maybe (SDL.Point SDL.V2 CInt)
    -> SDL.V2 Bool
    -> SDL.Font.Color
    -> SDL.Font.Color
    -> SDL.Font
    -> TVar FontCache
    -> CInt
    -> Text
    -> m ()
drawRectSprite' renderer texture sprite dstRect theta centre flips rectColor textColor font cache size text = do
    drawRectSprite renderer texture sprite dstRect theta centre flips
    when debug do
        SDL.rendererDrawColor renderer $=! rectColor
        SDL.drawRect renderer (Just dstRect)
        SDL.rendererDrawColor renderer $=! textColor
        let SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w _) = dstRect
        drawText
            renderer
            font
            cache
            (SDL.V4 0 0 0 255)
            text
            Text.Left
            Top
            (rect x y w size)

getPlayerTexture :: (MonadIO m) => SDL.Renderer -> m SDL.Texture
getPlayerTexture renderer = SDL.decodeTexture renderer $(embedFileRelative "assets/player.png")

getPlatformTexture :: (MonadIO m) => SDL.Renderer -> m SDL.Texture
getPlatformTexture renderer = SDL.decodeTexture renderer $(embedFileRelative "assets/platforms.png")

getSkyTexture :: (MonadIO m) => SDL.Renderer -> m SDL.Texture
getSkyTexture renderer = SDL.decodeTexture renderer $(embedFileRelative "assets/sky.png")

data PlayerSprite
    = Idle {frame :: Int}
    | Movement {frame :: Int}
    | Attack {frame :: Int}
    | Throw {frame :: Int}
    | Damage {frame :: Int}
    | Death {frame :: Int}
    deriving stock (Show)

instance RectSprite PlayerSprite where
    tileSize = 32
    spriteRect :: PlayerSprite -> SDL.Rectangle CInt
    spriteRect tile = rect (scale x) (scale y) (scale w) (scale h)
      where
        (x, y) = playerCoords tile
        (w, h) = (1, 1) :: (CInt, CInt)
        scale = (* tileSize @PlayerSprite)

playerCoords :: (Num a) => PlayerSprite -> (a, a)
playerCoords Idle{..} = (fromIntegral $ frame `mod` 5, 0)
playerCoords Movement{..} = (fromIntegral $ frame `mod` 8, 1)
playerCoords Attack{..} = (fromIntegral $ frame `mod` 4, 2)
playerCoords Throw{..} = (fromIntegral $ frame `mod` 5, 3)
playerCoords Damage{..} = (fromIntegral $ frame `mod` 4, 4)
playerCoords Death{..} = (fromIntegral $ frame `mod` 7, 5)

data PlatformSprite
    = FloatingBigLeft
    | FloatingBigCentre
    | FloatingBigRight
    | Floating1
    | Floating2
    | Floating3
    | Floating5
    deriving stock (Show)

instance RectSprite PlatformSprite where
    tileSize = 16
    spriteRect :: PlatformSprite -> SDL.Rectangle CInt
    spriteRect tile = rect (scale x) (scale y) (scale w) (scale h)
      where
        (x, y, w, h) = platformCoords tile
        scale = (* tileSize @PlatformSprite)

platformCoords :: (Num a) => PlatformSprite -> (a, a, a, a)
platformCoords FloatingBigLeft = (20, 2, 4, 7)
platformCoords FloatingBigCentre = (25, 2, 1, 7)
platformCoords FloatingBigRight = (27, 2, 4, 7)
platformCoords Floating1 = (33, 3, 3, 3)
platformCoords Floating2 = (37, 3, 4, 3)
platformCoords Floating3 = (3, 7, 3, 3)
platformCoords Floating5 = (2, 1, 7, 6)
