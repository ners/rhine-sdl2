module Simulate where

import App
import Data.List (find)
import SDL qualified
import Prelude

simulateS :: (Monad m, Diff (Time cl) ~ Double) => ClSF m cl AppState AppState
simulateS = sinceLastS &&& returnA >>> arr (uncurry simulate)

simulate :: Double -> AppState -> AppState
simulate dt state = state & changeDirection . accelerateY . moveY . moveX
  where
    moveX appState =
        let dx = appState.player.velocity.x * Tiles dt
         in appState & #player . #worldPosition . #x +~ dx
    moveY appState =
        let dy = appState.player.velocity.y * Tiles dt
         in appState & #player . #worldPosition . #y +~ dy
    terminal :: Tiles -> Tiles -> Tiles
    terminal terminalVelocity = clamp (-terminalVelocity, terminalVelocity)
    accelerateY appState@AppState{world = World{..}} =
        appState
            & #player
            %~ case playerOnGround appState of
                Nothing ->
                    let g = terminal terminalVelocity . (+ gravity * Tiles dt)
                     in #velocity . #y %~ g
                Just groundLevel ->
                    #velocity . #y .~ 0 >>> #worldPosition . #y .~ groundLevel
    changeDirection :: AppState -> AppState
    changeDirection appState =
        appState
            & #player
            . filtered ((0 <) . abs . (.velocity.x))
            . #facing
            .~ (if appState.player.velocity.x > 0 then FacingRight else FacingLeft)
    playerOnGround :: AppState -> Maybe Tiles
    playerOnGround AppState{player = Player{..}}
        | SDL.nearZero velocity.y && SDL.nearZero velocity.x = Just worldPosition.y
    playerOnGround newState =
        let oldY = state.player.worldPosition.y
            newY = newState.player.worldPosition.y
            state' = newState & #player . #worldPosition . #y .~ oldY
         in findPlatformUnderPlayer state' >>= \Platform{worldPosition = Pos{y}} ->
                if y >= oldY && y <= newY
                    then Just y
                    else Nothing

findPlatformUnderPlayer :: AppState -> Maybe Platform
findPlatformUnderPlayer AppState{..} = find (`isPlatformUnderPlayer` player) world.platforms

isPlatformUnderPlayer :: Platform -> Player -> Bool
isPlatformUnderPlayer platform player =
    player.worldPosition.x + 1 >= platform.worldPosition.x
        && player.worldPosition.x + 1
            <= platform.worldPosition.x + fromIntegral platform.width
        && player.worldPosition.y <= platform.worldPosition.y

distanceToPlatformUnderPlayer :: AppState -> Tiles
distanceToPlatformUnderPlayer appState@AppState{..} =
    case findPlatformUnderPlayer appState of
        Nothing -> 1 / 0
        Just platform -> platform.worldPosition.y - player.worldPosition.y
