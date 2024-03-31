module Main where

import Data.Bits (Bits (xor))
import Data.Foldable (for_)
import Data.Hashable (Hashable (hash))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (clamp)
import Data.Set (Set)
import Debug.Trace (traceShow)
import FRP.Rhine
import FRP.Rhine.SDL (InitConfig (..), RenderT, flowSDL, getRenderer, getWindow)
import Foreign.C (CInt (CInt))
import SDL qualified
import SDL.Primitive qualified as SDL
import System.Random (Random (random), mkStdGen)
import Prelude

data Pos = Pos {x :: Integer, y :: Integer} deriving stock (Eq, Ord, Show)

offsetPos :: (Integer -> Integer) -> (Integer -> Integer) -> Pos -> Pos
offsetPos fx fy Pos{..} = Pos{x = fx x, y = fy y}

sdlPos :: Pos -> SDL.Pos
sdlPos Pos{..} = SDL.V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y)

data Tile = Bomb | Normal Int
    deriving stock (Eq, Ord, Show)

data State = State
    { seed :: Int
    , bombDensity :: Float
    , offset :: Pos
    , cursor :: Maybe Pos
    , tileSize :: Integer
    , flags :: Set Pos
    , opened :: Map Pos Tile
    }

isBomb :: State -> Pos -> Bool
isBomb State{..} Pos{..} = (fst . random . mkStdGen . hash $ (seed, x, y)) < bombDensity

tile :: State -> Pos -> Tile
tile state pos
    | isBomb state pos = Bomb
    | otherwise = Normal $ bombNeighbours state pos

neighbours :: Pos -> [Pos]
neighbours pos =
    [ Pos{x, y}
    | x <- [pos.x - 1 .. pos.x + 1]
    , y <- [pos.y - 1 .. pos.y + 1]
    , Pos{x, y} /= pos
    ]

bombNeighbours :: State -> Pos -> Int
bombNeighbours state = length . filter (isBomb state) . neighbours

type SimClock = Millisecond 10

-- 60 FPS => 1000/60 ms/frame
type RenderClock = Millisecond 16

handleEvent
    :: forall m cl. (MonadIO m, Tag cl ~ SDL.Event) => ClSF m cl State State
handleEvent =
    tagS &&& returnA >>^ \(ev, st) -> case ev.eventPayload of
        SDL.QuitEvent -> undefined -- TODO: use ExceptT instead
        SDL.MouseButtonEvent
            (SDL.MouseButtonEventData{mouseButtonEventPos = SDL.P (SDL.V2 x y), ..})
                | mouseButtonEventMotion == SDL.Pressed ->
                    let tilePos = screenPosToTilePos st (Pos (fromIntegral x) (fromIntegral y))
                     in st{opened = Map.insert tilePos (tile st tilePos) st.opened}
        SDL.MouseMotionEvent
            (SDL.MouseMotionEventData{mouseMotionEventPos = SDL.P (SDL.V2 x y)}) ->
                st
                    { cursor = Just . screenPosToTilePos st $ Pos (fromIntegral x) (fromIntegral y)
                    }
        SDL.WindowLostMouseFocusEvent{} -> clearCursor st
        SDL.MouseWheelEvent
            (SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction}) ->
                let tileSize =
                        clamp (20, 100) $
                            st.tileSize + fromIntegral direction * max 1 (st.tileSize `div` 10)
                    x = tileSize * st.offset.x `div` st.tileSize
                    y = tileSize * st.offset.y `div` st.tileSize
                 in clearCursor $
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
                | code == 79 -> moveOffset (subtract st.tileSize) id st
                | code == 80 -> moveOffset (+ st.tileSize) id st
                | code == 81 -> moveOffset id (subtract st.tileSize) st
                | code == 82 -> moveOffset id (+ st.tileSize) st
        _ -> traceShow ev st
  where
    clearCursor :: State -> State
    clearCursor st = st{cursor = Nothing}
    moveOffset :: (Integer -> Integer) -> (Integer -> Integer) -> State -> State
    moveOffset fx fy st = clearCursor $ st{offset = offsetPos fx fy st.offset}

simulate :: (MonadIO m) => ClSF m cl State State
simulate = returnA

screenPosToTilePos :: State -> Pos -> Pos
screenPosToTilePos State{..} Pos{..} =
    Pos
        { x = (x - offset.x) `div` tileSize
        , y = (y - offset.y) `div` tileSize
        }

-- | This returns the top-left corner of the tile.
tilePosToScreenPos :: State -> Pos -> Pos
tilePosToScreenPos State{..} Pos{..} =
    Pos
        { x = x * tileSize + offset.x
        , y = y * tileSize + offset.y
        }

renderTile :: (MonadIO m) => SDL.Renderer -> State -> Pos -> m ()
renderTile renderer state pos =
    SDL.fillRectangle
        renderer
        (sdlPos topLeft)
        (sdlPos bottomRight)
        (colour opacity)
  where
    topLeft = tilePosToScreenPos state pos
    bottomRight = offsetPos (+ (state.tileSize - 1)) (+ (state.tileSize - 1)) topLeft
    opacity = if Just pos == state.cursor then 255 else 200
    colour
        | Just _ <- Map.lookup pos state.opened = SDL.V4 166 227 161
        | isBomb state pos = SDL.V4 250 179 135
        | even pos.x `xor` even pos.y = SDL.V4 132 170 245
        | otherwise = SDL.V4 137 180 250

renderFrame :: (MonadIO m) => ClSF (RenderT m) cl State ()
renderFrame = arrMCl \state -> do
    window <- getWindow
    (w, h) <-
        (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
            <$> SDL.get (SDL.windowSize window)
    let topLeftTile = screenPosToTilePos state (Pos 0 0)
    renderer <- getRenderer
    for_ [0 .. 1 + w `div` state.tileSize] $ \dx ->
        for_ [0 .. 1 + h `div` state.tileSize] $ \dy ->
            renderTile renderer state $ offsetPos (+ dx) (+ dy) topLeftTile

main :: IO ()
main = do
    let seed = 4 :: Int -- determined by a fair dice roll; guaranteed to be random
    let bombDensity = 0.1 :: Float
    flowSDL @IO @SimClock @RenderClock
        InitConfig
            { windowTitle = "rhine-sdl2 example"
            , windowConfig = SDL.defaultWindow{SDL.windowResizable = True}
            , rendererConfig = SDL.defaultRenderer
            , initialState =
                State
                    { seed
                    , bombDensity
                    , offset = Pos{x = 16, y = 16}
                    , cursor = Nothing
                    , tileSize = 32
                    , flags = mempty
                    , opened = mempty
                    }
            , handleEvent
            , simulate
            , renderFrame
            }
