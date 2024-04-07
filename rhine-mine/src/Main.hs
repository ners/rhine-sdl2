module Main where

import App
import Event
import FRP.Rhine.SDL (flowSDL)
import Render
import SDL qualified
import Simulate (simulateS)
import Sprite qualified
import System.Exit (exitWith)
import System.Random (randomIO)
import Prelude

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
        simulateS
        renderClock
        renderFrameS
        >>= exitWith
