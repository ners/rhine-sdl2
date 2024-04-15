{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Rhine.SDL where

import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Automaton.MSF.Trans.Except (reactimateExcept, try)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import FRP.Rhine hiding (EventClock, try)
import SDL qualified
import SDL.Raw qualified
import Prelude

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
    :: ( MonadIO m
       , MonadSchedule m
       , Time eventCl ~ time
       , Time (In eventCl) ~ time
       , Time (Out eventCl) ~ time
       , Time simCl ~ time
       , Time (In simCl) ~ time
       , Time (Out simCl) ~ time
       , Time renderCl ~ time
       , Time (In renderCl) ~ time
       , Time (Out renderCl) ~ time
       , GetClockProxy eventCl
       , GetClockProxy simCl
       , GetClockProxy renderCl
       , Clock m eventCl
       , Clock m (In eventCl)
       , Clock m (Out eventCl)
       , Clock m simCl
       , Clock m (In simCl)
       , Clock m (Out simCl)
       , Clock m renderCl
       , Clock m (In renderCl)
       , Clock m (Out renderCl)
       )
    => state
    -> renderState
    -> Rhine (ExceptT e m) eventCl state state
    -> Rhine m simCl state state
    -> Rhine m renderCl (state, renderState) renderState
    -> m e
flowSDL initialState renderState eventRh simRh renderRh =
    flowExcept $
        feedbackRhine
            (keepLast initialState)
            (feedbackify eventRh |@| feedbackify simRh)
            >-- keepLast initialState
            --> feedbackRhine
                (keepLast renderState)
                (renderRh @>>^ ((),))

feedbackify :: (Monad m) => Rhine m cl a a -> Rhine m cl ((), a) (a, a)
feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))

flowExcept
    :: ( Monad m
       , Clock (ExceptT e m) cl
       , GetClockProxy cl
       )
    => Rhine (ExceptT e m) cl () ()
    -> m e
flowExcept rhine =
    runExceptT (eraseClock rhine) >>= \case
        Left e -> pure e
        Right msf -> reactimateExcept . try $ msf >>> arr (const ())
