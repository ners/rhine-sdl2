module Main where

import Data.Hashable (Hashable (hash))
import Data.Ord (clamp)
import Data.Sequence (Seq, ViewL ((:<)), (<|))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShowM)
import FRP.Rhine
import FRP.Rhine.SDL (flowSDL)
import Foreign.C (CInt (CInt))
import SDL qualified
import SDL.Primitive qualified as SDL
import SDL.Raw qualified
import Sprite qualified
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Random (Random (random), mkStdGen, randomIO)
import Prelude

data Pos = Pos {x :: Integer, y :: Integer} deriving stock (Eq, Ord, Show)

offsetPos :: (Integer -> Integer) -> (Integer -> Integer) -> Pos -> Pos
offsetPos fx fy Pos{..} = Pos{x = fx x, y = fy y}

sdlPos :: Pos -> SDL.Pos
sdlPos Pos{..} = SDL.V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y)

fromPos :: (Num a) => (a -> a -> b) -> Pos -> b
fromPos f Pos{..} = f (fromIntegral x) (fromIntegral y)

toPos :: (Integral a) => a -> a -> Pos
toPos x y = Pos (fromIntegral x) (fromIntegral y)

data Tile = Bomb | Normal Int
    deriving stock (Eq, Ord, Show)

data AppState = AppState
    { seed :: Int
    , bombDensity :: Float
    , offset :: Pos
    , cursor :: Maybe Pos
    , tileSize :: Integer
    , flags :: Set Pos
    , opened :: Set Pos
    , tileOpenQueue :: Seq Pos
    }

clearCursor :: AppState -> AppState
clearCursor st = st{cursor = Nothing}

moveOffset
    :: (Integer -> Integer)
    -> (Integer -> Integer)
    -> AppState
    -> AppState
moveOffset fx fy st = clearCursor $ st{offset = offsetPos fx fy st.offset}

firstClickSeedReroll :: AppState -> Pos -> Int
firstClickSeedReroll st pos
    | Set.null st.opened && tile st pos == Bomb =
        firstClickSeedReroll st{seed = st.seed + 1} pos
    | otherwise = st.seed

data RenderState = RenderState
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , sprite :: SDL.Texture
    }

isBomb :: AppState -> Pos -> Bool
isBomb AppState{..} Pos{..} = (fst . random . mkStdGen . hash $ (seed, x, y)) < bombDensity

tile :: AppState -> Pos -> Tile
tile state pos
    | isBomb state pos = Bomb
    | otherwise = Normal $ adjacentBombs state pos

adjacentTiles :: Pos -> [Pos]
adjacentTiles pos =
    [ Pos{x, y}
    | x <- [pos.x - 1 .. pos.x + 1]
    , y <- [pos.y - 1 .. pos.y + 1]
    , Pos{x, y} /= pos
    ]

adjacentBombs :: AppState -> Pos -> Int
adjacentBombs state = length . filter (isBomb state) . adjacentTiles

handleEvent
    :: forall m
     . (MonadIO m)
    => SDL.Event
    -> AppState
    -> ExceptT ExitCode m AppState
handleEvent ev st =
    case ev.eventPayload of
        SDL.QuitEvent -> throwE ExitSuccess
        SDL.MouseButtonEvent
            ( SDL.MouseButtonEventData
                    { mouseButtonEventPos = SDL.P (SDL.V2 x y)
                    , mouseButtonEventMotion = SDL.Released
                    , ..
                    }
                )
                | mouseButtonEventButton == SDL.ButtonLeft ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                        seed = firstClickSeedReroll st tilePos
                     in pure st{tileOpenQueue = tilePos <| st.tileOpenQueue, seed}
                | mouseButtonEventButton == SDL.ButtonMiddle ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                        seed = firstClickSeedReroll st tilePos
                        tiles = Seq.fromList $ tilePos : adjacentTiles tilePos
                     in pure st{tileOpenQueue = tiles <> st.tileOpenQueue, seed}
                | mouseButtonEventButton == SDL.ButtonRight ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                        updateSet = if Set.member tilePos st.flags then Set.delete else Set.insert
                     in pure st{flags = updateSet tilePos st.flags}
        SDL.MouseMotionEvent
            (SDL.MouseMotionEventData{mouseMotionEventPos = SDL.P (SDL.V2 x y)}) ->
                pure
                    st
                        { cursor = Just . screenPosToTilePos st $ toPos x y
                        }
        SDL.WindowLostMouseFocusEvent{} -> pure $ clearCursor st
        SDL.MouseWheelEvent
            (SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction}) ->
                let tileSize =
                        clamp (20, 150) $
                            st.tileSize + fromIntegral direction * max 1 (st.tileSize `div` 10)
                    x = tileSize * st.offset.x `div` st.tileSize
                    y = tileSize * st.offset.y `div` st.tileSize
                 in pure . clearCursor $
                        st
                            { tileSize
                            , offset = Pos x y
                            }
        SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                    { keyboardEventKeyMotion = SDL.Pressed
                    , keyboardEventKeysym =
                        SDL.Keysym{keysymScancode = SDL.Scancode{unwrapScancode = code}}
                    }
                )
                | code == 79 -> pure $ moveOffset (subtract movementPx) id st
                | code == 80 -> pure $ moveOffset (+ movementPx) id st
                | code == 81 -> pure $ moveOffset id (subtract movementPx) st
                | code == 82 -> pure $ moveOffset id (+ movementPx) st
        _ -> traceShowM ev >> pure st
  where
    movementPx = 10 :: Integer

handleEventS
    :: (MonadIO m, Tag cl ~ SDL.Event)
    => ClSFExcept m cl AppState AppState ExitCode
handleEventS = try $ tagS &&& returnA >>> arrMCl (uncurry handleEvent)

simulate :: (MonadIO m) => ClSF m cl AppState AppState
simulate = arrMCl \state -> pure
    case Seq.viewl state.tileOpenQueue of
        Seq.EmptyL -> state
        pos :< remainingQueue
            | Set.member pos state.opened || Set.member pos state.flags ->
                state{tileOpenQueue = remainingQueue}
        pos :< remainingQueue ->
            let tileToOpen = tile state pos
                neighboursToOpen =
                    Seq.fromList
                        [ npos | tileToOpen == Normal 0, npos <- adjacentTiles pos, not (Set.member npos state.opened)
                        ]
             in state
                    { opened = Set.insert pos state.opened
                    , tileOpenQueue = remainingQueue <> neighboursToOpen
                    }

screenPosToTilePos :: AppState -> Pos -> Pos
screenPosToTilePos AppState{..} Pos{..} =
    Pos
        { x = (x - offset.x) `div` tileSize
        , y = (y - offset.y) `div` tileSize
        }

-- | This returns the top-left corner of the tile.
tilePosToScreenPos :: AppState -> Pos -> Pos
tilePosToScreenPos AppState{..} Pos{..} =
    Pos
        { x = x * tileSize + offset.x
        , y = y * tileSize + offset.y
        }

renderTile :: (MonadIO m) => RenderState -> AppState -> Pos -> m ()
renderTile RenderState{..} state pos = do
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

main :: IO ()
main = do
    SDL.initializeAll
    window <-
        SDL.createWindow "Rhine Mine" SDL.defaultWindow{SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    seed <- randomIO
    let bombDensity = 0.2 :: Float
    let tileSize = 64 :: Integer
    sprite <- Sprite.spriteTexture renderer
    let simClock = waitClock @10
    let renderClock = waitClock @16
    flowSDL
        AppState
            { seed
            , bombDensity
            , offset = Pos{x = tileSize `div` 2, y = tileSize `div` 2}
            , cursor = Nothing
            , tileSize
            , flags = mempty
            , opened = mempty
            , tileOpenQueue = mempty
            }
        RenderState{..}
        handleEventS
        simClock
        simulate
        renderClock
        renderFrame
        >>= exitWith
