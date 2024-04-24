{-# LANGUAGE DuplicateRecordFields #-}

module App where

import Dev
import Prelude

data Platform = Platform
    { worldPosition :: Coordinates Tiles
    , width :: CInt -- number of tiles, supported: 1, 2, 3, 5, >8
    }
    deriving stock (Generic, Show)

data World = World
    { gravity :: Tiles
    , terminalVelocity :: Tiles
    , platforms :: [Platform]
    }
    deriving stock (Generic, Show)

data FacingDirection = FacingLeft | FacingRight
    deriving stock (Generic, Eq, Show)

data Player = Player
    { worldPosition :: Coordinates Tiles
    , windowPosition :: Coordinates Double
    , facing :: FacingDirection
    , jumpVelocity :: Tiles
    , moveVelocity :: Tiles
    , velocity :: Coordinates Tiles
    }
    deriving stock (Generic, Show)

data AppState = AppState
    { seed :: Int
    , tileSize :: CInt
    , world :: World
    , player :: Player
    , dev :: Dev
    }
    deriving stock (Generic, Show)
