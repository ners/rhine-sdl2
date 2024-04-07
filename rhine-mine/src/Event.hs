module Event where

import App
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import SDL qualified
import System.Exit (ExitCode (ExitSuccess))
import Prelude

handleEvent
    :: forall m
     . (MonadIO m)
    => SDL.Event
    -> AppState
    -> ExceptT ExitCode m AppState
handleEvent ev st =
    case ev.eventPayload of
        SDL.QuitEvent -> throwE ExitSuccess
        SDL.MouseButtonEvent
            ( SDL.MouseButtonEventData
                    { mouseButtonEventPos = SDL.P (SDL.V2 x y)
                    , mouseButtonEventMotion = SDL.Released
                    , ..
                    }
                )
                | mouseButtonEventButton == SDL.ButtonLeft ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                        seed = firstClickSeedReroll st tilePos
                     in pure st{tileOpenQueue = tilePos <| st.tileOpenQueue, seed}
                | mouseButtonEventButton == SDL.ButtonMiddle ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                        seed = firstClickSeedReroll st tilePos
                        tiles = Seq.fromList $ tilePos : adjacentTiles tilePos
                     in pure st{tileOpenQueue = tiles <> st.tileOpenQueue, seed}
                | mouseButtonEventButton == SDL.ButtonRight ->
                    let tilePos = screenPosToTilePos st (toPos x y)
                        updateSet = if Set.member tilePos st.flags then Set.delete else Set.insert
                     in pure st{flags = updateSet tilePos st.flags}
        SDL.MouseMotionEvent
            (SDL.MouseMotionEventData{mouseMotionEventPos = SDL.P (SDL.V2 x y)}) ->
                pure
                    st
                        { cursor = Just . screenPosToTilePos st $ toPos x y
                        }
        SDL.WindowLostMouseFocusEvent{} -> pure $ clearCursor st
        SDL.MouseWheelEvent
            (SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction}) ->
                let tileSize =
                        clamp (20, 150) $
                            st.tileSize + fromIntegral direction * max 1 (st.tileSize `div` 10)
                    x = tileSize * st.offset.x `div` st.tileSize
                    y = tileSize * st.offset.y `div` st.tileSize
                 in pure . clearCursor $
                        st
                            { tileSize
                            , offset = Pos x y
                            }
        SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                    { keyboardEventKeyMotion = SDL.Pressed
                    , keyboardEventKeysym =
                        SDL.Keysym{keysymScancode = SDL.Scancode{unwrapScancode = code}}
                    }
                )
                | code == 79 -> pure $ moveOffset (subtract movementPx) id st
                | code == 80 -> pure $ moveOffset (+ movementPx) id st
                | code == 81 -> pure $ moveOffset id (subtract movementPx) st
                | code == 82 -> pure $ moveOffset id (+ movementPx) st
        _ -> traceShowM ev >> pure st
  where
    movementPx = 10 :: Integer

handleEventS
    :: (MonadIO m, Tag cl ~ SDL.Event)
    => ClSFExcept m cl AppState AppState ExitCode
handleEventS = try $ tagS &&& returnA >>> arrMCl (uncurry handleEvent)
