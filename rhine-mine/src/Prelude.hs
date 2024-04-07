module Prelude
    ( module Prelude
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.Extra
    , module Control.Monad.Reader
    , module Control.Monad.State.Strict
    , module Control.Monad.Trans.Class
    , module Data.Ord
    , module Data.Sequence
    , module Data.Set
    , module Debug.Trace
    , module FRP.Rhine
    , module Foreign.C
    , module GHC.Generics
    )
where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Extra hiding (loop)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.State.Strict
    ( MonadState
    , StateT (..)
    , evalStateT
    , execStateT
    , runStateT
    )
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Generics.Labels ()
import Data.Ord (clamp)
import Data.Sequence (Seq, ViewL (..))
import Data.Set (Set)
import Debug.Trace
import FRP.Rhine
import Foreign.C (CInt (CInt))
import GHC.Generics (Generic)
import SDL qualified
import SDL.Raw qualified
import "base" Prelude

data Pos = Pos {x :: Integer, y :: Integer}
    deriving stock (Generic, Eq, Ord, Show)

toSdlPos :: forall a. (Integral a) => Lens' Pos (SDL.V2 a)
toSdlPos = lens getter setter
  where
    getter :: Pos -> SDL.V2 a
    getter Pos{..} = SDL.V2 (fromIntegral x) (fromIntegral y)
    setter :: Pos -> SDL.V2 a -> Pos
    setter _ = view fromSdlPos

fromSdlPos :: forall a. (Integral a) => Lens' (SDL.V2 a) Pos
fromSdlPos = lens getter setter
  where
    getter :: SDL.V2 a -> Pos
    getter (SDL.V2 (fromIntegral -> x) (fromIntegral -> y)) = Pos{..}
    setter :: SDL.V2 a -> Pos -> SDL.V2 a
    setter _ = view toSdlPos

fpoint :: Lens' Pos SDL.Raw.FPoint
fpoint = lens getter setter
  where
    getter :: Pos -> SDL.Raw.FPoint
    getter Pos{..} = SDL.Raw.FPoint{fPointX = fromIntegral x, fPointY = fromIntegral y}
    setter :: Pos -> SDL.Raw.FPoint -> Pos
    setter _ SDL.Raw.FPoint{..} = Pos{x = round fPointX, y = round fPointY}

adjacentTiles :: Pos -> [Pos]
adjacentTiles pos =
    [ Pos{x, y}
    | x <- [pos.x - 1 .. pos.x + 1]
    , y <- [pos.y - 1 .. pos.y + 1]
    , Pos{x, y} /= pos
    ]

data Tile = Bomb | Normal Int
    deriving stock (Eq, Ord, Show)
