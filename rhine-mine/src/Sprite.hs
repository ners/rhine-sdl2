{-# LANGUAGE TemplateHaskell #-}

module Sprite where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import FRP.Rhine
import Foreign.C (CFloat)
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Raw qualified
import Prelude

spriteContents :: ByteString
spriteContents = $(embedFileRelative "assets/sprite.png")

-- | A sprite sheet with rectangular images
class RectSprite a where
    spriteRect :: a -> SDL.Raw.FRect

spriteTexture :: (MonadIO m) => SDL.Renderer -> m SDL.Texture
spriteTexture renderer = SDL.decodeTexture renderer spriteContents

data Tile
    = Uncovered Int
    | Covered
    | Bomb
    | Flag

instance RectSprite Sprite.Tile where
    spriteRect :: Sprite.Tile -> SDL.Raw.FRect
    spriteRect tile = SDL.Raw.FRect (x / cols) (y / rows) (1 / cols) (1 / rows)
      where
        (x, y) = tileCoords tile
        rows = 3 :: CFloat
        cols = 4 :: CFloat

tileCoords :: (Num a) => Sprite.Tile -> (a, a)
tileCoords (Uncovered i)
    | i >= 1 && i < 5 = (fromIntegral i - 1, 0)
    | i >= 5 && i < 9 = (fromIntegral i - 5, 1)
    | i == 0 = (0, 2)
    | otherwise =
        error $ "spriteCoords: Uncovered " <> show i <> " is out of bounds"
tileCoords Sprite.Covered = (1, 2)
tileCoords Sprite.Bomb = (2, 2)
tileCoords Sprite.Flag = (3, 2)

drawRectSprite
    :: (MonadIO m, RectSprite sprite)
    => SDL.Renderer
    -> SDL.Texture
    -> sprite
    -> SDL.Raw.FRect
    -> m ()
drawRectSprite renderer texture sprite dstRect = SDL.renderGeometry renderer (Just texture) vertices ixs
  where
    srcRect = spriteRect sprite
    blendColour = SDL.Raw.Color 255 255 255 255
    vertex f = SDL.Vertex (f dstRect) blendColour (f srcRect)
    vertices :: Vector SDL.Vertex
    vertices =
        Vector.fromList
            [vertex topLeft, vertex topRight, vertex bottomRight, vertex bottomLeft]
    ixs :: Vector CInt
    ixs = Vector.fromList [2, 1, 0, 2, 0, 3]

leftX, rightX, topY, bottomY :: SDL.Raw.FRect -> CFloat
leftX (SDL.Raw.FRect x _ _ _) = x
rightX (SDL.Raw.FRect x _ w _) = x + w
topY (SDL.Raw.FRect _ y _ _) = y
bottomY (SDL.Raw.FRect _ y _ h) = y + h

topLeft, topRight, bottomLeft, bottomRight :: SDL.Raw.FRect -> SDL.Raw.FPoint
topLeft r = SDL.Raw.FPoint (leftX r) (topY r)
topRight r = SDL.Raw.FPoint (rightX r) (topY r)
bottomLeft r = SDL.Raw.FPoint (leftX r) (bottomY r)
bottomRight r = SDL.Raw.FPoint (rightX r) (bottomY r)
