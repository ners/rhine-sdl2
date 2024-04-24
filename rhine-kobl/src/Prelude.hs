{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Prelude
    ( module Prelude
    , module Control.Concurrent.STM
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.State.Strict
    , module Data.ByteString
    , module Data.FileEmbed
    , module Data.Foldable
    , module Data.Map
    , module Data.Ord
    , module Data.Sequence
    , module Data.Set
    , module Data.Text
    , module Data.Vector.Storable
    , module Data.Word
    , module Debug.Trace
    , module FRP.Rhine
    , module Foreign.C
    , module GHC.Generics
    , module GHC.Stack
    , module SDL
    )
where

import Control.Concurrent.STM
import Control.Lens.Combinators hiding (Level)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.State.Strict (StateT, runStateT)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Ord (clamp)
import Data.Sequence (Seq, ViewL (..))
import Data.Set (Set)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import Data.Word
import Debug.Trace
import FRP.Rhine
import Foreign.C
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import SDL (($=!))
import SDL qualified
import Text.Printf (printf)
import "base" Prelude

debug :: Bool
#ifdef DEBUG
debug = True
#else
debug = False
#endif

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

newtype Tiles = Tiles {unTiles :: Double}
    deriving newtype (Eq, Ord, Num, Fractional, Real, RealFrac, SDL.Epsilon)

globalPos :: forall i. (Integral i) => i -> Iso' Tiles Double
globalPos (fromIntegral -> tileSize) = iso sa bt
  where
    sa :: Tiles -> Double
    sa = (tileSize *) . (.unTiles)
    bt :: Double -> Tiles
    bt = Tiles . (/ tileSize)

windowPos :: forall i. (Integral i) => i -> Double -> Iso' Tiles i
windowPos (globalPos -> global) globalOffset = iso sa bt
  where
    sa :: Tiles -> i
    sa t = round $ (t ^. global) - globalOffset
    bt :: i -> Tiles
    bt i = (fromIntegral i + globalOffset) ^. from global

instance Show Tiles where
    show = printf "%0.1f" . (.unTiles)

data Coordinates a = Pos {x :: a, y :: a}
    deriving stock (Generic, Eq, Ord, Show)

translateCoords
    :: forall a b
     . Iso' a b
    -> Iso' a b
    -> Iso' (Coordinates a) (Coordinates b)
translateCoords ix iy = iso ab ba
  where
    ab :: Coordinates a -> Coordinates b
    ab Pos{..} = Pos{x = x ^. ix, y = y ^. iy}
    ba :: Coordinates b -> Coordinates a
    ba Pos{..} = Pos{x = x ^. from ix, y = y ^. from iy}

offsetCoords :: (a -> b) -> (a -> b) -> Coordinates a -> Coordinates b
offsetCoords fx fy Pos{..} = Pos{x = fx x, y = fy y}

instance (Num a) => Num (Coordinates a) where
    a + b = Pos{x = a.x + b.x, y = a.y + b.y}
    a - b = Pos{x = a.x - b.x, y = a.y - b.y}
    a * b = Pos{x = a.x * b.x, y = a.y * b.y}
    abs Pos{..} = Pos{x = abs x, y = abs y}
    fromInteger a = Pos{x = fromInteger a, y = fromInteger a}
    signum Pos{..} = Pos{x = signum x, y = signum y}

instance (Fractional a) => Fractional (Coordinates a) where
    a / b = Pos{x = a.x / b.x, y = a.y / b.y}
    fromRational a = Pos{x = fromRational a, y = fromRational a}

rect :: a -> a -> a -> a -> SDL.Rectangle a
rect x y w h = SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 w h)

rectTuple :: Lens' (SDL.Rectangle a) (a, a, a, a)
rectTuple = lens get set
  where
    get :: SDL.Rectangle a -> (a, a, a, a)
    get (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)) = (x, y, w, h)
    set :: SDL.Rectangle a -> (a, a, a, a) -> SDL.Rectangle a
    set _ (x, y, w, h) = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)

rectContains :: (Num a, Ord a) => SDL.Point SDL.V2 a -> SDL.Rectangle a -> Bool
rectContains (SDL.P (SDL.V2 x y)) (view rectTuple -> (rx, ry, rw, rh)) =
    x >= rx
        && x <= rx + rw
        && y >= ry
        && y <= ry + rh
