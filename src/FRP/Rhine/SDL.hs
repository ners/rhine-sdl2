{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Rhine.SDL where

import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Word (Word32)
import FRP.Rhine hiding (EventClock)
import SDL (($=))
import SDL qualified
import SDL.Raw qualified
import Prelude
import GHC.TypeNats (KnownNat)

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

flowSDL
    :: forall m simCl renderCl sim render cl st
     . ( MonadIO m
       , MonadSchedule m
       , Clock m simCl
       , Clock m renderCl
       , KnownNat sim
       , simCl ~ Millisecond sim
       , KnownNat render
       , renderCl ~ Millisecond render
       , cl ~ ParallelClock EventClock simCl
       )
    => st
    -> ClSF m EventClock st st
    -> ClSF m simCl st st
    -> ClSF m renderCl (st, SDL.Renderer, SDL.Window) ()
    -> m ()
flowSDL initialState handleEvent simulate renderFrame = do
    SDL.initializeAll
    window <- SDL.createWindow "Title" SDL.defaultWindow{SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    let renderRh :: Rhine m renderCl (st, SDL.Renderer, SDL.Window) ()
        renderRh = render @@ waitClock
    flow $
        handleEventOrSim
            >-- keepLast initialState
            --> (,renderer,window)
                ^>>@ renderRh
    SDL.destroyWindow window
    SDL.quit
  where
    eventRh :: Rhine m EventClock st st
    eventRh = handleEvent @@ EventClock
    simRh :: Rhine m simCl st st
    simRh = simulate @@ waitClock
    handleEventOrSim :: Rhine m cl () st
    handleEventOrSim =
        feedbackRhine
            (keepLast initialState)
            (feedbackify eventRh |@| feedbackify simRh)
    feedbackify :: Rhine m cl' a a -> Rhine m cl' ((), a) (a, a)
    feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))
    preRender, postRender :: SDL.Renderer -> m ()
    preRender renderer = do
        SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
        SDL.clear renderer
    postRender renderer = do
        SDL.present renderer
    render :: ClSF m renderCl (st, SDL.Renderer, SDL.Window) ()
    render = proc x@(_, renderer, _) -> do
        arrMCl preRender -< renderer
        renderFrame -< x
        arrMCl postRender -< renderer
