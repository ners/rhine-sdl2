module Event where

import App
import Data.Int (Int32)
import Data.List qualified as List
import Dev
import SDL qualified
import System.Exit (ExitCode (ExitSuccess))
import Prelude hiding (EventClock)

data Event
    = Exit
    | Esc
    | Jump Tiles
    | Move Tiles
    | Zoom Int32
    | FocusDevPanel Int
    | FocusGame
    deriving stock (Show)

fromSdlEvent :: AppState -> SDL.Event -> Maybe Event
fromSdlEvent AppState{..} event = case event.eventPayload of
    SDL.QuitEvent -> Just Exit
    SDL.KeyboardEvent
        SDL.KeyboardEventData
            { keyboardEventKeyMotion
            , keyboardEventRepeat = False
            , keyboardEventKeysym =
                SDL.Keysym{keysymScancode = SDL.Scancode{unwrapScancode = code}}
            }
            | code == 41 && keyboardEventKeyMotion == SDL.Pressed ->
                Just Esc
            | not dev.focused && code == 44 && keyboardEventKeyMotion == SDL.Pressed ->
                Just $ Jump (-player.jumpVelocity)
            | not dev.focused && code == 79 && keyboardEventKeyMotion == SDL.Pressed ->
                Just $ Move player.moveVelocity
            | not dev.focused && code == 80 && keyboardEventKeyMotion == SDL.Pressed ->
                Just $ Move (-player.moveVelocity)
            | not dev.focused && code == 79 && keyboardEventKeyMotion == SDL.Released ->
                Just $ Move (-player.moveVelocity)
            | not dev.focused && code == 80 && keyboardEventKeyMotion == SDL.Released ->
                Just $ Move player.moveVelocity
    SDL.MouseWheelEvent
        SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction} ->
            Just $ Zoom direction
    SDL.MouseButtonEvent
        SDL.MouseButtonEventData{mouseButtonEventPos = SDL.P (SDL.V2 x y)} | dev.open -> do
            maybe (Just FocusGame) (Just . FocusDevPanel) $
                List.findIndex
                    (rectContains (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) . (.rectangle))
                    dev.panels
    _ -> Nothing

handleEvent
    :: (Monad m)
    => SDL.Event
    -> AppState
    -> ExceptT ExitCode m AppState
handleEvent ev st = case fromSdlEvent st ev of
    Nothing -> pure st
    Just Exit -> throwE ExitSuccess
    Just Esc -> pure $ st & #dev . #open %~ not
    Just (Jump vy) -> pure $ st & #player . #velocity . #y . filtered SDL.nearZero +~ vy
    Just (Move vx) -> pure $ st & #player . #velocity . #x +~ vx
    Just (Zoom direction) ->
        pure
            st
                { tileSize =
                    clamp (10, 150) $
                        st.tileSize + fromIntegral direction * max 1 (st.tileSize `div` 10)
                }
    Just (FocusDevPanel i) -> pure $ st & #dev %~ (#focused .~ True) . (#panels . ix i . #focused .~ True)
    Just FocusGame ->
        pure $
            st & #dev %~ (#focused .~ False) . (#panels . traverse . #focused .~ False)

handleEventS
    :: (Monad m, Tag cl ~ SDL.Event)
    => ClSFExcept cl AppState AppState m ExitCode
handleEventS = try $ tagS &&& returnA >>> arrMCl (uncurry handleEvent)
