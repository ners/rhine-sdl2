module Render where

import App
import SDL qualified
import Prelude
import SDL.Primitive qualified as SDL

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    }

renderTile :: (MonadIO m) => RenderState -> AppState -> Pos -> m ()
renderTile RenderState{..} state pos = do
    case tile state pos of
        Wall -> SDL.fillRectangle renderer topLeft bottomRight (SDL.V4 0 0 0 255)
        Air -> SDL.fillRectangle renderer topLeft bottomRight (SDL.V4 255 255 255 255)
  where
    screenPos = tilePosToScreenPos state pos
    topLeft  = fromPos SDL.V2 screenPos
    bottomRight = fromPos SDL.V2 $ offsetPos (+ state.tileSize) (+ state.tileSize) screenPos

renderFrameS :: (MonadIO m) => ClSF m cl (AppState, RenderState) RenderState
renderFrameS = arrMCl $ uncurry renderFrame

renderFrame :: (MonadIO m) => AppState -> RenderState -> m RenderState
renderFrame state r@RenderState{..} = do
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
