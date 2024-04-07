{-# LANGUAGE TemplateHaskell #-}

module Sprite where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import FRP.Rhine
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Raw qualified
import Prelude

spriteContents :: ByteString
spriteContents = $(embedFileRelative "assets/sprite.png")

spriteSurface :: (MonadIO m) => m SDL.Surface
spriteSurface = SDL.decode spriteContents

spriteTexture :: (MonadIO m) => SDL.Renderer -> m SDL.Texture
spriteTexture renderer = SDL.decodeTexture renderer spriteContents

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
spriteRect (Sprite.Uncovered i)
    | i >= 1 && i < 5 = tileRect 0 (i - 1)
    | i >= 5 && i < 9 = tileRect 1 (i - 5)
    | i == 0 = tileRect 2 0
    | otherwise = error $ "spriteRect: Uncovered " <> show i <> " is out of bounds"
spriteRect Sprite.Covered = tileRect 2 1
spriteRect Sprite.Bomb = tileRect 2 2
spriteRect Sprite.Flag = tileRect 2 3

surfaceDrawSprite
    :: (MonadIO m)
    => SDL.Surface
    -> Sprite
    -> SDL.Surface
    -> SDL.Rectangle CInt
    -> m ()
surfaceDrawSprite src sprite dst dstPos = SDL.surfaceBlitScaled src (Just $ spriteRect sprite) dst (Just dstPos)

{-
0 1
3 2
-}

tileCoords :: Int -> Int -> SDL.V4 SDL.Raw.FPoint
tileCoords y x = SDL.V4 topLeft topRight bottomRight bottomLeft
  where
    topLeft = SDL.Raw.FPoint leftX topY
    topRight = SDL.Raw.FPoint rightX topY
    bottomRight = SDL.Raw.FPoint rightX bottomY
    bottomLeft = SDL.Raw.FPoint leftX bottomY
    spriteCols, spriteRows :: (Num a) => a
    spriteCols = 4
    spriteRows = 3
    leftX = fromIntegral x / spriteCols
    rightX = fromIntegral (x + 1) / spriteCols
    topY = fromIntegral y / spriteRows
    bottomY = fromIntegral (y + 1) / spriteRows

spriteCoords :: Sprite -> SDL.V4 SDL.Raw.FPoint
spriteCoords (Uncovered i)
    | i >= 1 && i < 5 = tileCoords 0 (i - 1)
    | i >= 5 && i < 9 = tileCoords 1 (i - 5)
    | i == 0 = tileCoords 2 0
    | otherwise =
        error $ "spriteCoords: Uncovered " <> show i <> " is out of bounds"
spriteCoords Sprite.Covered = tileCoords 2 1
spriteCoords Sprite.Bomb = tileCoords 2 2
spriteCoords Sprite.Flag = tileCoords 2 3

textureDrawSprite
    :: (MonadIO m)
    => SDL.Renderer
    -> SDL.Texture
    -> Sprite
    -> SDL.V4 SDL.Raw.FPoint
    -> m ()
textureDrawSprite renderer texture sprite dstPos = SDL.renderGeometry renderer (Just texture) vertices ixs
  where
    (SDL.V4 srcTopLeft srcTopRight srcBottomRight srcBottomLeft) = spriteCoords sprite
    (SDL.V4 dstTopLeft dstTopRight dstBottomRight dstBottomLeft) = dstPos
    topLeft, topRight, bottomLeft, bottomRight :: SDL.Vertex
    topLeft = SDL.Vertex dstTopLeft blendColour srcTopLeft
    topRight = SDL.Vertex dstTopRight blendColour srcTopRight
    bottomLeft = SDL.Vertex dstBottomLeft blendColour srcBottomLeft
    bottomRight = SDL.Vertex dstBottomRight blendColour srcBottomRight
    blendColour = SDL.Raw.Color 255 255 255 255
    vertices :: Vector SDL.Vertex
    vertices = Vector.fromList [topLeft, topRight, bottomRight, bottomLeft]
    ixs :: Vector CInt
    ixs = Vector.fromList [2, 1, 0, 2, 0, 3]
