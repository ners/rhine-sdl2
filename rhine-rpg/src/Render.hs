module Render where

import App
import SDL qualified
import SDL.Primitive qualified as SDL
import Prelude

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    }

renderTile :: (MonadIO m) => RenderState -> AppState -> Pos -> Tile -> m ()
renderTile RenderState{..} state pos tile = do
    case tile of
        Wall -> SDL.fillRectangle renderer topLeft bottomRight (SDL.V4 0 0 0 255)
        Door -> SDL.fillRectangle renderer topLeft bottomRight (SDL.V4 0 0 255 255)
        Air -> SDL.fillRectangle renderer topLeft bottomRight (SDL.V4 255 255 255 255)
  where
    screenPos = tilePosToScreenPos state pos
    topLeft = fromPos SDL.V2 screenPos
    bottomRight = fromPos SDL.V2 $ offsetPos (+ state.tileSize) (+ state.tileSize) screenPos

renderFrameS :: (MonadIO m) => ClSF m cl (AppState, RenderState) RenderState
renderFrameS = arrMCl $ uncurry renderFrame

renderFrame :: (MonadIO m) => AppState -> RenderState -> m RenderState
renderFrame state r@RenderState{..} = do
    -- (w, h) <-
    --    (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
    --        <$> SDL.get (SDL.windowSize window)
    -- let _topLeftTile = screenPosToTilePos state (Pos 0 0)

    mapM_
        ( \(i, row) -> do
            let inner = zip [0 ..] row
            mapM_
                ( \(j, cell) -> do
                    let pos = Pos{x = i, y = j}
                    renderTile r state pos cell
                )
                inner
        )
        $ zip [0 ..] state.gameMap

    SDL.present renderer
    pure r
