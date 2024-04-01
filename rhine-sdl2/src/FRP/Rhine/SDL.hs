{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Rhine.SDL where

import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Automaton.MSF.Trans.Except (reactimateExcept, try)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import FRP.Rhine hiding (EventClock, try)
import GHC.TypeNats (KnownNat)
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
    :: forall m simCl renderCl sim render st a
     . ( MonadIO m
       , MonadSchedule m
       , Clock m simCl
       , Clock m renderCl
       , KnownNat sim
       , simCl ~ Millisecond sim
       , KnownNat render
       , renderCl ~ Millisecond render
       )
    => st
    -> ClSFExcept m EventClock st st a
    -> ClSF m simCl st st
    -> ClSF m renderCl st ()
    -> m a
flowSDL initialState handleEvent simulate render = do
    flowExcept $
        feedbackRhine
            (keepLast initialState)
            ( feedbackify (runClSFExcept handleEvent @@ EventClock)
                |@| feedbackify (liftClSFAndClock simulate @@ liftClock waitClock)
            )
            >-- keepLast initialState
            --> liftClSFAndClock render @@ liftClock waitClock

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
