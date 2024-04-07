module App where

import Data.Hashable (Hashable (hash))
import Data.Set qualified as Set
import System.Random (Random (random), mkStdGen)
import Prelude

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
    deriving stock (Generic)

type AppT = StateT AppState

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

isBomb :: AppState -> Pos -> Bool
isBomb AppState{..} Pos{..} = (fst . random . mkStdGen . hash $ (seed, x, y)) < bombDensity

tile :: AppState -> Pos -> Tile
tile state pos
    | isBomb state pos = Bomb
    | otherwise = Normal $ adjacentBombs state pos

adjacentBombs :: AppState -> Pos -> Int
adjacentBombs state = length . filter (isBomb state) . adjacentTiles

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
