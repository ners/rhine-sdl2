module Render where

import App
import SDL qualified
import Sprite qualified
import Prelude

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , sprite :: SDL.Texture
    }
    deriving stock (Generic)

newtype RenderT m a = Render {unRender :: StateT RenderState (AppT m) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader AppState
        , MonadState RenderState
        )

runRenderT
    :: (Monad m) => RenderT m a -> (AppState, RenderState) -> m (a, RenderState)
runRenderT Render{..} (appState, renderState) = evalAppT (runStateT unRender renderState) appState

evalRenderT :: (Monad m) => RenderT m a -> (AppState, RenderState) -> m a
evalRenderT Render{..} (appState, renderState) = evalAppT (evalStateT unRender renderState) appState

execRenderT
    :: (Monad m) => RenderT m a -> (AppState, RenderState) -> m RenderState
execRenderT Render{..} (appState, renderState) = evalAppT (execStateT unRender renderState) appState

instance MonadTrans RenderT where
    lift = Render . lift . lift

renderTile :: (MonadIO m, MonadIO (RenderT m)) => Pos -> RenderT m ()
renderTile pos = do
    renderer <- use #renderer
    sprite <- use #sprite

    spriteType <-
        ifM
            (isOpened pos)
            ( tile pos <&> \case
                Bomb -> Sprite.Bomb
                Normal i -> Sprite.Uncovered i
            )
            (ifM (isFlagged pos) (pure Sprite.Flag) (pure Sprite.Covered))
    tileSize <- view #tileSize
    topLeft <- tilePosToScreenPos pos
    let topRight = topLeft & #x +~ tileSize
    let bottomRight = topRight & #y +~ tileSize
    let bottomLeft = topLeft & #y +~ tileSize

    Sprite.textureDrawSprite
        renderer
        sprite
        spriteType
        ( SDL.V4
            (topLeft ^. fpoint)
            (topRight ^. fpoint)
            (bottomRight ^. fpoint)
            (bottomLeft ^. fpoint)
        )

renderFrame :: (MonadIO m, MonadIO (RenderT m)) => RenderT m ()
renderFrame = do
    window <- use #window
    (w, h) <-
        (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
            <$> SDL.get (SDL.windowSize window)
    topLeftTile <- screenPosToTilePos $ Pos 0 0 ^. toSdlPos @Int
    tileSize <- view #tileSize
    mapM_
        renderTile
        [ #x %~ (+) dx >>> #y %~ (+) dy $ topLeftTile
        | dx <- [0 .. 1 + w `div` tileSize]
        , dy <- [0 .. 1 + h `div` tileSize]
        ]
    SDL.present =<< use #renderer

renderFrameS :: (MonadIO m) => ClSF m cl (AppState, RenderState) RenderState
renderFrameS = arrMCl $ execRenderT renderFrame
