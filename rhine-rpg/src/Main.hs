module Main where

import App
import Event
import FRP.Rhine.SDL (flowSDL)
import ModelSynthesis
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
    let tileSize :: Int = 8
    let gen =
            GenParams
                { seed = 42
                , maxSpeed = 6
                , maxJumpSpeed = 3
                , gravity = 9.81
                , maximalDrop = 3
                , minGapWidth = 2
                , maxGapWidth = 5
                }
    let level = ModelSynthesis.generate gen 10
    flowSDL
        AppState
            { seed
            , tileSize
            , cursor = Nothing
            , offset = Pos 20 20
            , level
            }
        RenderState{..}
        handleEventS
        simClock
        simulateS
        renderClock
        renderFrameS
        >>= exitWith
