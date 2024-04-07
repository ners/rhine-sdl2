module Prelude
    ( module Prelude
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad.State.Strict
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
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Generics.Labels ()
import Data.Ord (clamp)
import Data.Sequence (Seq, ViewL (..))
import Data.Set (Set)
import Debug.Trace
import FRP.Rhine
import Foreign.C (CInt (CInt))
import GHC.Generics (Generic)
import SDL qualified
import SDL.Primitive qualified as SDL
import "base" Prelude

data Pos = Pos {x :: Integer, y :: Integer} deriving stock (Eq, Ord, Show)

offsetPos :: (Integer -> Integer) -> (Integer -> Integer) -> Pos -> Pos
offsetPos fx fy Pos{..} = Pos{x = fx x, y = fy y}

adjacentTiles :: Pos -> [Pos]
adjacentTiles pos =
    [ Pos{x, y}
    | x <- [pos.x - 1 .. pos.x + 1]
    , y <- [pos.y - 1 .. pos.y + 1]
    , Pos{x, y} /= pos
    ]

sdlPos :: Pos -> SDL.Pos
sdlPos Pos{..} = SDL.V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y)

fromPos :: (Num a) => (a -> a -> b) -> Pos -> b
fromPos f Pos{..} = f (fromIntegral x) (fromIntegral y)

toPos :: (Integral a) => a -> a -> Pos
toPos x y = Pos (fromIntegral x) (fromIntegral y)

data Tile = Bomb | Normal Int
    deriving stock (Eq, Ord, Show)
