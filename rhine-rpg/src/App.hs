module App where

import Prelude

data AppState = AppState
    { seed :: Int
    , tileSize :: Integer
    , offset :: Pos
    , cursor :: Maybe Pos
    , gameMap :: [[Tile]]
    }
    deriving stock (Generic)

clearCursor :: AppState -> AppState
clearCursor st = st{cursor = Nothing}

moveOffset
    :: (Pos -> Pos)
    -> AppState
    -> AppState
moveOffset moveBy st = clearCursor $ st{offset = moveBy st.offset}

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
