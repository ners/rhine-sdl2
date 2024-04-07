module Render where

import App
import Data.Set qualified as Set
import SDL qualified
import SDL.Raw qualified
import Sprite qualified
import Prelude

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , sprite :: SDL.Texture
    }
    deriving stock (Generic)

type RenderT m = StateT RenderState (ReaderT AppState m)

renderTile :: (MonadIO m) => Pos -> RenderT m ()
renderTile pos = do
    renderer <- use #renderer
    sprite <- use #sprite

    spriteType <- isOpened pos

    Sprite.textureDrawSprite
        renderer
        sprite
        spriteType
        (SDL.V4 topLeft topRight bottomRight bottomLeft)
  where
    spriteType
        | Set.member pos state.opened = case tile state pos of
            Bomb -> Sprite.Bomb
            Normal i -> Sprite.Uncovered i
        | Set.member pos state.flags = Sprite.Flag
        | otherwise = Sprite.Covered
    screenPos = tilePosToScreenPos state pos
    topLeft = fromPos SDL.Raw.FPoint screenPos
    topRight = fromPos SDL.Raw.FPoint $ offsetPos (+ state.tileSize) id screenPos
    bottomRight =
        fromPos SDL.Raw.FPoint $
            offsetPos (+ state.tileSize) (+ state.tileSize) screenPos
    bottomLeft = fromPos SDL.Raw.FPoint $ offsetPos id (+ state.tileSize) screenPos

renderFrame :: (MonadIO m) => ClSF m cl (AppState, RenderState) RenderState
renderFrame = arrMCl \(state, r@RenderState{..}) -> do
    (w, h) <-
        (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
            <$> SDL.get (SDL.windowSize window)
    let topLeftTile = screenPosToTilePos state (Pos 0 0)
    mapM_
        (renderTile r state)
        [ offsetPos (+ dx) (+ dy) topLeftTile
        | dx <- [0 .. 1 + w `div` state.tileSize]
        , dy <- [0 .. 1 + h `div` state.tileSize]
        ]
    SDL.present renderer
    pure r
