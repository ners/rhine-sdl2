{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dev where

import SDL qualified
import Prelude

data Panel = Panel
    { rectangle :: SDL.Rectangle CInt
    , focused :: Bool
    , title :: Text
    }
    deriving stock (Generic, Show)

data Dev = Dev
    { open :: Bool
    , focused :: Bool
    , panels :: [Panel]
    }
    deriving stock (Generic, Show)
