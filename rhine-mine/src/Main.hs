module Main where

import FRP.Rhine.SDL (flowSDL)
import SDL qualified
import Sprite qualified
import System.Random (randomIO)
import Prelude
import App
import Event
import Render
import System.Exit (exitWith)

main :: IO ()
main = do
    SDL.initializeAll
    window <-
        SDL.createWindow "Rhine Mine" SDL.defaultWindow{SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    seed <- randomIO
    let bombDensity = 0.2 :: Float
    let tileSize = 64 :: Integer
    sprite <- Sprite.spriteTexture renderer
    let simClock = waitClock @10
    let renderClock = waitClock @16
    flowSDL
        AppState
            { seed
            , bombDensity
            , offset = Pos{x = tileSize `div` 2, y = tileSize `div` 2}
            , cursor = Nothing
            , tileSize
            , flags = mempty
            , opened = mempty
            , tileOpenQueue = mempty
            }
        RenderState{..}
        handleEventS
        simClock
        simulate
        renderClock
        renderFrame
        >>= exitWith
