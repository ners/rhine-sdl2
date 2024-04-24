{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Render where

import App
import Control.Monad.State qualified as State
import Dev
import Font
import SDL qualified
import SDL.Font qualified
import Simulate (distanceToPlatformUnderPlayer)
import Sprite
import Text
import Widget
import Prelude

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , fonts :: Fonts
    , platformTexture :: SDL.Texture
    , playerTexture :: SDL.Texture
    , skyTexture :: SDL.Texture
    }

renderS
    :: (MonadIO m, Diff (Time cl) ~ Double)
    => ClSF m cl (AppState, RenderState) RenderState
renderS =
    sinceInitS
        &&& returnA
        >>> arrMCl (\(time, (appState, renderState)) -> render time appState renderState)

data Camera = Camera
    { topLeft :: Coordinates Double
    , windowWidth :: CInt
    , windowHeight :: CInt
    , tileSize :: CInt
    }

getCamera :: (MonadIO m) => AppState -> RenderState -> m Camera
getCamera AppState{..} RenderState{..} = do
    SDL.V2 windowWidth windowHeight <- SDL.get (SDL.windowSize window)
    let playerWorld = player.worldPosition ^. globalCoordinates tileSize
        topLeft =
            Pos
                { x = playerWorld.x - player.windowPosition.x * fromIntegral windowWidth
                , y = playerWorld.y - player.windowPosition.y * fromIntegral windowHeight
                }
    pure Camera{..}

globalCoordinates :: CInt -> Iso' (Coordinates Tiles) (Coordinates Double)
globalCoordinates (globalPos -> global) = translateCoords global global

windowCoordinates :: Camera -> Iso' (Coordinates Tiles) (Coordinates CInt)
windowCoordinates Camera{..} = translateCoords (windowPos tileSize topLeft.x) (windowPos tileSize topLeft.y)

render :: (MonadIO m) => Double -> AppState -> RenderState -> m RenderState
render time appState@AppState{..} renderState@RenderState{..} = do
    camera <- getCamera appState renderState
    renderSky renderState camera
    for_ world.platforms $ renderPlatform camera renderState
    renderPlayer camera appState renderState player time
    when debug $ renderGrid camera renderState
    when dev.open $ renderDev appState renderState
    SDL.present renderer
    pure renderState

renderSky :: (MonadIO m) => RenderState -> Camera -> m ()
renderSky RenderState{..} Camera{..} = do
    SDL.rendererDrawColor renderer $=! SDL.V4 225 250 233 255
    SDL.clear renderer
    for_ [0, w .. windowWidth] \x ->
        SDL.copy renderer skyTexture Nothing . Just $ rect x 0 w windowHeight
  where
    srcW, srcH, w :: CInt
    (srcW, srcH) = (112, 304)
    h = windowHeight
    w = h * srcW `div` srcH

renderPlayer
    :: (MonadIO m)
    => Camera
    -> AppState
    -> RenderState
    -> Player
    -> Double
    -> m ()
renderPlayer Camera{..} appState RenderState{..} player time = do
    let size, x, y :: CInt
        size = 2 * tileSize
        x = round $ player.windowPosition.x * fromIntegral windowWidth
        y =
            round $
                player.windowPosition.y * fromIntegral windowHeight - fromIntegral size / 1.7
    let sprite
            | abs player.velocity.y > 0 =
                let numFrames = 3 :: Int
                    frames = [5, 6, 7] :: [Int]
                    f = round (player.velocity.y / player.jumpVelocity) `mod` numFrames
                 in Movement (frames !! f)
            | abs player.velocity.x > 0 = Movement (round $ time * 12)
            | otherwise = Idle (round $ time * 6)
    drawRectSprite'
        renderer
        playerTexture
        sprite
        (rect x y size size)
        0
        Nothing
        (SDL.V2 (player.facing == FacingLeft) False)
        (SDL.V4 200 0 0 255)
        (SDL.V4 0 0 0 255)
        fonts.cherry13r.font
        fonts.cherry13r.cache
        fonts.cherry13r.size
        (ishow (player.worldPosition.x, player.worldPosition.y))
    when debug $
        drawText
            renderer
            fonts.cherry13r.font
            fonts.cherry13r.cache
            (SDL.V4 0 0 0 255)
            ( ishow
                (player.velocity.x, player.velocity.y, distanceToPlatformUnderPlayer appState)
            )
            Text.Left
            Top
            (rect x (y + fonts.cherry13r.size) size fonts.cherry13r.size)

renderPlatform :: (MonadIO m) => Camera -> RenderState -> Platform -> m ()
renderPlatform camera@Camera{..} RenderState{..} Platform{..}
    | width < 8 =
        drawRectSprite'
            renderer
            platformTexture
            sprite
            (rect topLeft.x topLeft.y (w * tileSize) (h * tileSize))
            0
            Nothing
            (SDL.V2 False False)
            (SDL.V4 0 200 0 255)
            (SDL.V4 0 0 0 255)
            fonts.cherry13r.font
            fonts.cherry13r.cache
            fonts.cherry13r.size
            (ishow (worldPosition.x, worldPosition.y))
  where
    (sprite, leftOffset) =
        case width of
            1 -> (Floating1, subtract 1)
            2 -> (Floating2, subtract 1)
            3 -> (Floating3, id)
            5 -> (Floating5, subtract 1)
            _ -> error $ "renderPlatform: unsupported platform width: " <> show width
    (_, _, w, h) = platformCoords @CInt sprite
    topLeft = offsetCoords leftOffset id worldPosition ^. windowCoordinates camera
renderPlatform camera@Camera{..} RenderState{..} Platform{..} =
    for_ pieces $ \(dx, w, h, piece) -> do
        let pieceTopLeft = offsetCoords (+ dx) id worldPosition ^. windowCoordinates camera

        drawRectSprite'
            renderer
            platformTexture
            piece
            (rect pieceTopLeft.x pieceTopLeft.y (w * tileSize) (h * tileSize))
            0
            Nothing
            (SDL.V2 False False)
            (SDL.V4 0 200 0 255)
            (SDL.V4 0 0 0 255)
            fonts.cherry13r.font
            fonts.cherry13r.cache
            fonts.cherry13r.size
            (ishow (worldPosition.x + dx, worldPosition.y))
  where
    (_, _, leftWidth, leftHeight) = platformCoords @CInt FloatingBigLeft
    (_, _, centreWidth, centreHeight) = platformCoords @CInt FloatingBigCentre
    (_, _, rightWidth, rightHeight) = platformCoords @CInt FloatingBigRight
    leftPiece = (0 :: Tiles, leftWidth, leftHeight, FloatingBigLeft)
    centrePieces =
        [ ( Tiles . fromIntegral $ leftWidth + i
          , centreWidth
          , centreHeight
          , FloatingBigCentre
          )
        | i <- [0, centreWidth .. width - leftWidth - rightWidth - 1]
        ]
    rightPiece =
        ( Tiles . fromIntegral $ width - rightWidth
        , rightWidth
        , rightHeight
        , FloatingBigRight
        )
    pieces = [leftPiece] <> centrePieces <> [rightPiece]

renderDev :: (MonadIO m) => AppState -> RenderState -> m ()
renderDev appState renderState = for_ appState.dev.panels $ renderPanel appState renderState

renderPanel :: (MonadIO m) => AppState -> RenderState -> Panel -> m ()
renderPanel AppState{..} RenderState{..} Panel{..} = do
    let (rx, ry, rw, _) = view rectTuple rectangle
    let headerRect = rect rx ry rw headerHeight
    -- bg
    SDL.rendererDrawColor renderer $=! bg
    SDL.fillRect renderer $ Just rectangle
    SDL.rendererDrawColor renderer $=! headerBg
    SDL.fillRect renderer $ Just headerRect

    -- outline
    SDL.rendererDrawColor renderer $=! outline
    SDL.drawRect renderer $ Just rectangle
    SDL.drawRect renderer $ Just headerRect

    let cx = rx + padding
    let contentWidth = rw - 2 * padding

    -- title
    drawText
        renderer
        fonts.cherry13b.font
        fonts.cherry13b.cache
        fg
        title
        Text.Left
        VCentre
        (rect cx ry contentWidth headerHeight)

    -- content
    flip State.evalStateT (ry + headerHeight + padding) do
        let runWidget :: (Widget w, State.MonadState CInt m, MonadIO m) => w -> m ()
            runWidget w = do
                y <- State.get
                dy <- widget renderer fonts (SDL.P $ SDL.V2 cx y) contentWidth w
                State.modify (+ dy)
        runWidget @Text "Hello world!"
        runWidget
            TextInput{label = "Player position", value = ishow player.worldPosition}
        runWidget Spacer
        runWidget TextInput{label = "Player name", value = "kobl"}
        runWidget Spacer
        runWidget Button{label = "Let's go!", width = 100}
  where
    padding = 8 :: CInt
    headerHeight = fonts.cherry13b.size + padding
    opacity = 230 :: Word8
    fg = SDL.V4 @Word8 255 255 255 opacity
    bg = SDL.V4 @Word8 30 30 60 opacity
    outline = SDL.V4 @Word8 128 128 128 255
    headerBg =
        if focused
            then SDL.V4 @Word8 100 10 10 opacity
            else SDL.V4 @Word8 50 50 100 opacity

renderGrid :: (MonadIO m) => Camera -> RenderState -> m ()
renderGrid Camera{..} RenderState{..} = do
    SDL.rendererDrawBlendMode renderer $=! SDL.BlendAlphaBlend
    SDL.rendererDrawColor renderer $=! SDL.V4 200 0 0 20
    for_ [0 .. tilesW] \((leftTile +) -> tx) -> do
        let x = Tiles (fromIntegral tx) ^. windowPos tileSize topLeft.x
        SDL.drawLine
            renderer
            (SDL.P $ SDL.V2 x 0)
            (SDL.P $ SDL.V2 x windowHeight)
        drawText
            renderer
            fonts.cherry13r.font
            fonts.cherry13r.cache
            black
            (ishow tx)
            Text.Left
            Top
            (rect x 0 tileSize fonts.cherry13r.size)
    for_ [0 .. tilesH] \((topTile +) -> ty) -> do
        let y = Tiles (fromIntegral ty) ^. windowPos tileSize topLeft.y
        SDL.drawLine
            renderer
            (SDL.P $ SDL.V2 0 y)
            (SDL.P $ SDL.V2 windowWidth y)
        drawText
            renderer
            fonts.cherry13r.font
            fonts.cherry13r.cache
            black
            (ishow ty)
            Text.Left
            Top
            (rect 0 y tileSize fonts.cherry13r.size)
  where
    global = globalPos tileSize
    leftTile = floor $ topLeft.x ^. from global
    tilesW = windowWidth `div` tileSize
    topTile = floor $ topLeft.y ^. from global
    tilesH = windowHeight `div` tileSize
    black = SDL.V4 0 0 0 255 :: SDL.Font.Color
