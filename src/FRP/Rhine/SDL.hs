{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Rhine.SDL where

import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Word (Word32)
import FRP.Rhine hiding (EventClock)
import SDL qualified
import SDL.Raw qualified
import Prelude

instance TimeDifference Word32 where
    add = (+)
    difference = (-)

instance TimeDomain SDL.Timestamp where
    type Diff SDL.Timestamp = Word32
    addTime = add
    diffTime = difference

data EventClock = EventClock

instance (MonadIO m) => Clock m EventClock where
    type Time EventClock = UTCTime
    type Tag EventClock = SDL.Event
    initClock :: EventClock -> RunningClockInit m (Time EventClock) (Tag EventClock)
    initClock EventClock = do
        initialSdlTime <- SDL.Raw.getTicks
        initialTime <- liftIO getCurrentTime
        let clock :: MSF m () (Time EventClock, Tag EventClock)
            clock = constM SDL.waitEvent >>^ (\e -> (eventTimestamp e, e))
            eventTimestamp :: SDL.Event -> UTCTime
            eventTimestamp e =
                addUTCTime
                    ( secondsToNominalDiffTime . (/ 1e6) . fromIntegral $
                        e.eventTimestamp - initialSdlTime
                    )
                    initialTime
        pure (clock, initialTime)

instance GetClockProxy EventClock

type SimulationClock = Millisecond 10

type FooClock = ParallelClock EventClock SimulationClock

-- | 60 FPS ~ 16 ms/frame
type RenderClock = Millisecond 16

flowSDL
    :: forall m st
     . ( MonadIO m
       , MonadSchedule m
       , Clock m SimulationClock
       , Clock m RenderClock
       )
    => st
    -> ClSF m EventClock st st
    -> ClSF m SimulationClock st st
    -> ClSF m RenderClock st ()
    -> m ()
flowSDL initialState handleEvent simulate renderFrame = do
    SDL.initializeAll
    window <- SDL.createWindow "Title" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    let renderRh :: Rhine m RenderClock st ()
        renderRh = render renderer @@ waitClock
    flow $
        handleEventOrSim
            >-- keepLast initialState
            --> renderRh
    SDL.destroyWindow window
    SDL.quit
  where
    eventRh :: Rhine m EventClock st st
    eventRh = handleEvent @@ EventClock
    simRh :: Rhine m SimulationClock st st
    simRh = simulate @@ waitClock
    handleEventOrSim :: Rhine m FooClock () st
    handleEventOrSim =
        feedbackRhine
            (keepLast initialState)
            (feedbackify eventRh |@| feedbackify simRh)
    feedbackify :: Rhine m cl a a -> Rhine m cl ((), a) (a, a)
    feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))
    render :: SDL.Renderer -> ClSF m RenderClock st ()
    render renderer =
        renderFrame >>> constM do
            SDL.clear renderer
            SDL.present renderer
