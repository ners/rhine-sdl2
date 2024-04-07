module Event where

import App
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import SDL (Event (eventPayload))
import SDL qualified
import System.Exit (ExitCode (ExitSuccess))
import Prelude

---- | Commute a 'StateT' layer past an 'ExceptT' layer.
-- commuteExceptState
--    :: (Monad m)
--    => ExceptT e (StateT s m) a
--    -> StateT s (ExceptT e m) a
-- commuteExceptState a = StateT $ \s -> ExceptT do
--    (e, s') <- runStateT (runExceptT a) s
--    pure $ e <&> (,s')
--
---- | Commute the effects of the 'StateT' and the 'ExceptT' monad.
-- commuteStateExcept
--    :: (Monad m)
--    => StateT s (ExceptT e m) a
--    -> ExceptT e (StateT s m) a
-- commuteStateExcept a = ExceptT $ StateT $ \s ->
--    runExceptT (runStateT a s) <&> \case
--        Left e -> (Left e, s)
--        Right (a', s') -> (Right a', s')

-- | Commute a 'AppT' layer past an 'ExceptT' layer.
commuteExceptApp :: (Monad m) => ExceptT e (AppT m) a -> AppT (ExceptT e m) a
commuteExceptApp a = App $ StateT $ \s -> ExceptT do
    (e, s') <- runAppT (runExceptT a) s
    pure $ e <&> (,s')

---- | Commute the effects of the 'AppT' and the 'ExceptT' monad.
-- commuteAppExcept :: (Monad m) => AppT (ExceptT e m) a -> ExceptT e (AppT m) a
-- commuteAppExcept a = ExceptT $ App $ StateT $ \s ->
--    runExceptT (runAppT a s) <&> \case
--        Left e -> (Left e, s)
--        Right (a', s') -> (Right a', s')

handleEventS
    :: (MonadIO m, Tag cl ~ SDL.Event)
    => ClSFExcept m cl AppState AppState ExitCode
handleEventS =
    try $
        tagS
            &&& returnA
            >>> arrMCl \(ev, st) ->
                execAppT (commuteExceptApp $ handleEvent ev) st

handleEvent
    :: forall m
     . (MutApp m)
    => SDL.Event
    -> ExceptT ExitCode m ()
handleEvent SDL.Event{eventPayload = SDL.QuitEvent} = throwE ExitSuccess
handleEvent
    SDL.Event
        { eventPayload =
            SDL.MouseButtonEvent
                ( SDL.MouseButtonEventData
                        { mouseButtonEventPos = SDL.P pos
                        , mouseButtonEventMotion = SDL.Released
                        , ..
                        }
                    )
        }
        | mouseButtonEventButton == SDL.ButtonLeft = do
            tilePos <- screenPosToTilePos pos
            firstClickSeedReroll tilePos
            modifying #tileOpenQueue (tilePos <|)
        | mouseButtonEventButton == SDL.ButtonMiddle = do
            tilePos <- screenPosToTilePos pos
            firstClickSeedReroll tilePos
            let tiles = Seq.fromList $ tilePos : adjacentTiles tilePos
            modifying #tileOpenQueue (tiles <>)
        | mouseButtonEventButton == SDL.ButtonRight = do
            tilePos <- screenPosToTilePos pos
            f <- isFlagged tilePos
            modifying #flags $ (if f then Set.delete else Set.insert) tilePos
handleEvent
    SDL.Event
        { eventPayload =
            SDL.MouseMotionEvent
                (SDL.MouseMotionEventData{mouseMotionEventPos = SDL.P pos})
        } =
        assign #cursor . Just =<< screenPosToTilePos pos
handleEvent SDL.Event{eventPayload = SDL.WindowLostMouseFocusEvent{}} = clearCursor
handleEvent
    SDL.Event
        { eventPayload =
            SDL.MouseWheelEvent
                (SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction})
        } = do
        oldTileSize <- use #tileSize
        let newTileSize =
                clamp (20, 150) $
                    oldTileSize + fromIntegral direction * max 1 (oldTileSize `div` 10)
        assign #tileSize newTileSize
        let scale x = newTileSize * x `div` oldTileSize
        modifying #offset $ #x %~ scale >>> #y %~ scale
handleEvent
    SDL.Event
        { eventPayload =
            SDL.KeyboardEvent
                ( SDL.KeyboardEventData
                        { keyboardEventKeyMotion = SDL.Pressed
                        , keyboardEventKeysym =
                            SDL.Keysym{keysymScancode = SDL.Scancode{unwrapScancode = code}}
                        }
                    )
        }
        | code == 79 = modifying #offset $ #x -~ movementPx
        | code == 80 = modifying #offset $ #x +~ movementPx
        | code == 81 = modifying #offset $ #y -~ movementPx
        | code == 82 = modifying #offset $ #y +~ movementPx
      where
        movementPx = 20 :: Integer
handleEvent ev = traceShowM ev
