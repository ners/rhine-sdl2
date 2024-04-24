module Main where

import App
import Dev
import Event
import FRP.Rhine.SDL (flowSDL)
import Font qualified
import Render
import SDL qualified
import SDL.Font qualified
import Simulate (simulateS)
import Sprite qualified
import System.Exit (exitWith)
import System.Random (randomIO)
import Prelude

main :: IO ()
main = do
    SDL.initializeAll
    SDL.Font.initialize
    seed <- randomIO
    let simClock = waitClock @10
    let renderClock = waitClock @16
    let initialState =
            AppState
                { seed
                , tileSize = 64
                , world =
                    World
                        { gravity = 80
                        , terminalVelocity = 30
                        , platforms =
                            [ Platform
                                { worldPosition = Pos{x = -5, y = 0}
                                , width = 10
                                }
                            , Platform
                                { worldPosition = Pos{x = 7, y = 1.3}
                                , width = 3
                                }
                            , Platform
                                { worldPosition = Pos{x = 12, y = 1}
                                , width = 8
                                }
                            , Platform
                                { worldPosition = Pos{x = 23, y = 1}
                                , width = 1
                                }
                            ]
                        }
                , player =
                    Player
                        { worldPosition = Pos{x = 0, y = 0}
                        , windowPosition = Pos{x = 0.5, y = 0.5}
                        , facing = FacingRight
                        , jumpVelocity = 20
                        , moveVelocity = 8
                        , velocity = Pos{x = 0, y = 0}
                        }
                , dev =
                    Dev
                        { open = False
                        , focused = False
                        , panels =
                            [ Panel
                                { rectangle = rect 30 30 500 300
                                , focused = False
                                , title = "Dev tools"
                                }
                            ]
                        }
                }
    window <-
        SDL.createWindow "kobl" SDL.defaultWindow{SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    fonts <- Font.getFonts 100
    platformTexture <- Sprite.getPlatformTexture renderer
    playerTexture <- Sprite.getPlayerTexture renderer
    skyTexture <- Sprite.getSkyTexture renderer
    let initialRenderState = RenderState{..}
    flowSDL
        initialState
        initialRenderState
        handleEventS
        simClock
        simulateS
        renderClock
        renderS
        >>= exitWith
