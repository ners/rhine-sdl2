module Event where

import App
import Data.Int (Int32)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import SDL qualified
import System.Exit (ExitCode (ExitSuccess))
import Prelude hiding (EventClock)

data Event
    = Exit
    | TileOpen Pos
    | TileMultiOpen Pos
    | ToggleFlag Pos
    | ChangeOffset (Pos -> Pos)
    | Zoom Int32 Pos
    | Hover (Maybe Pos)

fromSdlEvent :: AppState -> SDL.Event -> Maybe Event
fromSdlEvent st event =
    case event.eventPayload of
        SDL.QuitEvent -> Just Exit
        SDL.MouseButtonEvent
            ( SDL.MouseButtonEventData
                    { mouseButtonEventPos = SDL.P (SDL.V2 x y)
                    , mouseButtonEventMotion = SDL.Released
                    , ..
                    }
                )
                | mouseButtonEventButton == SDL.ButtonLeft ->
                    Just . TileOpen . screenPosToTilePos st $ toPos x y
                | mouseButtonEventButton == SDL.ButtonMiddle ->
                    Just . TileMultiOpen . screenPosToTilePos st $ toPos x y
                | mouseButtonEventButton == SDL.ButtonRight ->
                    Just . ToggleFlag . screenPosToTilePos st $ toPos x y
        SDL.MouseMotionEvent
            (SDL.MouseMotionEventData{mouseMotionEventPos = SDL.P (SDL.V2 x y)}) ->
                Just . Hover . Just . screenPosToTilePos st $ toPos x y
        SDL.WindowLostMouseFocusEvent{} -> Just $ Hover Nothing
        SDL.MouseWheelEvent
            (SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction}) ->
                Zoom direction <$> st.cursor
        SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                    { keyboardEventKeyMotion = SDL.Pressed
                    , keyboardEventKeysym =
                        SDL.Keysym{keysymScancode = SDL.Scancode{unwrapScancode = code}}
                    }
                )
                | code == 79 -> changeOffset (subtract movementPx) id
                | code == 80 -> changeOffset (+ movementPx) id
                | code == 81 -> changeOffset id (subtract movementPx)
                | code == 82 -> changeOffset id (+ movementPx)
              where
                movementPx = 10 :: Integer
                changeOffset updateX updateY =
                    Just $ ChangeOffset $ \p -> Pos{x = updateX p.x, y = updateY p.y}
        _ -> Nothing

handleEvent
    :: forall m
     . (MonadIO m)
    => SDL.Event
    -> AppState
    -> ExceptT ExitCode m AppState
handleEvent ev st = case fromSdlEvent st ev of
    Nothing -> pure st
    Just Exit -> throwE ExitSuccess
    Just (TileOpen tilePos) ->
        let seed = firstClickSeedReroll st tilePos
         in pure st{tileOpenQueue = tilePos <| st.tileOpenQueue, seed}
    Just (TileMultiOpen tilePos) ->
        let seed = firstClickSeedReroll st tilePos
            tiles = Seq.fromList $ tilePos : adjacentTiles tilePos
         in pure st{tileOpenQueue = tiles <> st.tileOpenQueue, seed}
    Just (ToggleFlag tilePos) ->
        let updateSet = if Set.member tilePos st.flags then Set.delete else Set.insert
         in pure st{flags = updateSet tilePos st.flags}
    Just (Hover maybePos) ->
        pure st{cursor = maybePos}
    Just (Zoom direction _pos) ->
        let tileSize =
                clamp (20, 150) $
                    st.tileSize + fromIntegral direction * max 1 (st.tileSize `div` 10)
            x = tileSize * st.offset.x `div` st.tileSize
            y = tileSize * st.offset.y `div` st.tileSize
         in pure
                st
                    { tileSize
                    , offset = Pos x y
                    }
    Just (ChangeOffset updatePos) ->
        pure $ moveOffset updatePos st

handleEventS
    :: (MonadIO m, Tag cl ~ SDL.Event)
    => ClSFExcept cl AppState AppState m ExitCode
handleEventS = try $ tagS &&& returnA >>> arrMCl (uncurry handleEvent)
