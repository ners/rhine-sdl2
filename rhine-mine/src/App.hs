module App where

import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
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

newtype AppT r m a = App {unApp :: StateT r m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadTrans
        , MonadState r
        )

instance (Monad m) => MonadReader r (AppT r m) where
    ask = State.get
    local f a = do
        oldState <- State.get
        let newState = f oldState
        State.put newState
        a <* State.put oldState

type ReadApp m = MonadReader AppState m

type MutApp m = (ReadApp m, MonadState AppState m)

isOpened :: (ReadApp m) => Pos -> m Bool
isOpened = views #opened . Set.member

isFlagged :: (ReadApp m) => Pos -> m Bool
isFlagged = views #flags . Set.member

isTouched :: (ReadApp m) => Pos -> m Bool
isTouched pos = orM [isOpened pos, isFlagged pos]

clearCursor :: (MutApp m) => m ()
clearCursor = assign #cursor Nothing

firstClickSeedReroll :: (MutApp m) => Pos -> m ()
firstClickSeedReroll pos = do
    firstClick <- uses #opened Set.null
    bomb <- isBomb pos
    when (firstClick && bomb) do
        modifying #seed (+ 1)
        firstClickSeedReroll pos

isBomb :: (ReadApp m) => Pos -> m Bool
isBomb Pos{..} = do
    AppState{..} <- Reader.ask
    pure $ (fst . random . mkStdGen . hash $ (seed, x, y)) < bombDensity

tile :: (ReadApp m) => Pos -> m Tile
tile pos = ifM (isBomb pos) (pure Bomb) (Normal <$> adjacentBombs pos)

adjacentBombs :: (ReadApp m) => Pos -> m Int
adjacentBombs pos = length <$> filterM isBomb (adjacentTiles pos)

screenPosToTilePos :: (ReadApp m) => Pos -> m Pos
screenPosToTilePos Pos{..} = do
    AppState{..} <- Reader.ask
    pure $
        Pos
            { x = (x - offset.x) `div` tileSize
            , y = (y - offset.y) `div` tileSize
            }

-- | This returns the top-left corner of the tile.
tilePosToScreenPos :: (ReadApp m) => Pos -> m Pos
tilePosToScreenPos Pos{..} = do
    AppState{..} <- Reader.ask
    pure $
        Pos
            { x = x * tileSize + offset.x
            , y = y * tileSize + offset.y
            }
