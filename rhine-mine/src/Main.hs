module Main where

import Data.Foldable (for_)
import Data.Hashable (Hashable (hash))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (clamp)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import FRP.Rhine
import FRP.Rhine.SDL (flowSDL)
import Foreign.C (CInt (CInt))
import SDL qualified
import SDL.Primitive qualified as SDL
import Sprite qualified
import System.Random (Random (random), mkStdGen)
import Prelude

data Pos = Pos {x :: Integer, y :: Integer} deriving stock (Eq, Ord, Show)

offsetPos :: (Integer -> Integer) -> (Integer -> Integer) -> Pos -> Pos
offsetPos fx fy Pos{..} = Pos{x = fx x, y = fy y}

sdlPos :: Pos -> SDL.Pos
sdlPos Pos{..} = SDL.V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y)

fromPos :: (Integral a) => (a -> a -> b) -> Pos -> b
fromPos f Pos{..} = f (fromIntegral x) (fromIntegral y)

toPos :: (Integral a) => a -> a -> Pos
toPos x y = Pos (fromIntegral x) (fromIntegral y)

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
    , sprite :: SDL.Surface
    , window :: SDL.Window
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
            (SDL.MouseButtonEventData{mouseButtonEventPos = SDL.P (SDL.V2 x y), mouseButtonEventMotion = SDL.Released, ..})
                | mouseButtonEventButton == SDL.ButtonLeft ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                     in st{opened = Map.insert tilePos (tile st tilePos) st.opened}
                | mouseButtonEventButton == SDL.ButtonRight ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                     in st{flags = Set.insert tilePos st.flags}
        SDL.MouseMotionEvent
            (SDL.MouseMotionEventData{mouseMotionEventPos = SDL.P (SDL.V2 x y)}) ->
                st
                    { cursor = Just . screenPosToTilePos st $ toPos x y
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

renderTile :: (MonadIO m) => SDL.Window -> State -> Pos -> m ()
renderTile window state pos = do
    surface <- SDL.getWindowSurface window
    let sprite
            | Just t <- Map.lookup pos state.opened = case t of
                Bomb -> Sprite.Bomb
                Normal i -> Sprite.Uncovered i
            | Set.member pos state.flags = Sprite.Flag
            | otherwise = Sprite.Covered
    Sprite.drawSprite
        state.sprite
        sprite
        surface
        ( SDL.Rectangle
            (SDL.P $ fromPos SDL.V2 $ tilePosToScreenPos state pos)
            (SDL.V2 (fromIntegral state.tileSize) (fromIntegral state.tileSize))
        )

renderFrame :: (MonadIO m) => ClSF m cl State ()
renderFrame = arrMCl \state -> do
    (w, h) <-
        (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
            <$> SDL.get (SDL.windowSize state.window)
    let topLeftTile = screenPosToTilePos state (Pos 0 0)
    for_ [0 .. 1 + w `div` state.tileSize] $ \dx ->
        for_ [0 .. 1 + h `div` state.tileSize] $ \dy ->
            renderTile state.window state $ offsetPos (+ dx) (+ dy) topLeftTile
    SDL.updateWindowSurface state.window

main :: IO ()
main = do
    SDL.initializeAll
    window <-
        SDL.createWindow "Rhine Mine" SDL.defaultWindow{SDL.windowResizable = True}
    let seed = 4 :: Int -- determined by a fair dice roll; guaranteed to be random
    let bombDensity = 0.1 :: Float
    let tileSize = 32 :: Integer
    sprite <- Sprite.getSprite
    flowSDL @IO @SimClock @RenderClock
        State
            { seed
            , bombDensity
            , offset = Pos{x = tileSize `div` 2, y = tileSize `div` 2}
            , cursor = Nothing
            , tileSize
            , flags = mempty
            , opened = mempty
            , sprite
            , window
            }
        handleEvent
        simulate
        renderFrame
