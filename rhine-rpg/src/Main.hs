module Main where

import App
import Event
import FRP.Rhine.SDL (flowSDL)
import Render
import SDL qualified
import Simulate (simulateS)
import System.Exit (exitWith)
import System.Random (randomIO)
import Prelude

main :: IO ()
main = do
    SDL.initializeAll
    window <-
        SDL.createWindow "Rhine RPG" SDL.defaultWindow{SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    seed <- randomIO
    -- sprite <- Sprite.spriteTexture renderer
    let simClock = waitClock @10
    let renderClock = waitClock @16
    let tileSize :: Integer = 10
    flowSDL
        AppState
            { seed
            , tileSize = tileSize
            , offset = Pos{x = tileSize `div` 2, y = tileSize `div` 2}
            , cursor = Nothing
            }
        RenderState{..}
        handleEventS
        simClock
        simulateS
        renderClock
        renderFrameS
        >>= exitWith
