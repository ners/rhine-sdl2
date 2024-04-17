module Render where

import App
import SDL qualified
import SDL.Primitive qualified as SDL
import Prelude

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    }

renderTile :: (MonadIO m) => RenderState -> AppState -> Floor -> m ()
renderTile RenderState{..} state fl = do
    SDL.fillRectangle renderer topLeft bottomRight (SDL.V4 128 128 128 255)
  where
    pos = Pos{x = fl.x, y=fl.y}
    screenPos = tilePosToScreenPos state pos
    topLeft = fromPos SDL.V2 screenPos
    bottomRight =
        fromPos SDL.V2 $
            tilePosToScreenPos state $
            offsetPos
                (+ fl.width)
                (+ 1)
                pos

renderFrameS :: (MonadIO m) => ClSF m cl (AppState, RenderState) RenderState
renderFrameS = arrMCl $ uncurry renderFrame

renderFrame :: (MonadIO m) => AppState -> RenderState -> m RenderState
renderFrame state r@RenderState{..} = do
    -- (w, h) <-
    --    (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
    --        <$> SDL.get (SDL.windowSize window)
    -- let _topLeftTile = screenPosToTilePos state (Pos 0 0)

    mapM_ (renderTile r state) state.level.floors
    SDL.present renderer
    pure r
