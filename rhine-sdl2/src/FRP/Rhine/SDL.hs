{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Rhine.SDL where

import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import FRP.Rhine hiding (EventClock)
import GHC.TypeNats (KnownNat)
import SDL (($=))
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

data RenderEnv = RenderEnv
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    }

newtype RenderT m a = RenderT { unRenderT :: ReaderT RenderEnv m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

getWindow :: (MonadIO m) => RenderT m SDL.Window
getWindow = RenderT $ asks (.window)

windowS :: (MonadIO m) => ClSF (RenderT m) cl a SDL.Window
windowS = constM $ lift getWindow

getRenderer :: (MonadIO m) => RenderT m SDL.Renderer
getRenderer = RenderT $ asks (.renderer)

rendererS :: (MonadIO m) => ClSF (RenderT m) cl a SDL.Renderer
rendererS = constM $ lift getRenderer

data InitConfig m st simCl renderCl = InitConfig
    { windowTitle :: Text
    , windowConfig :: SDL.WindowConfig
    , rendererConfig :: SDL.RendererConfig
    , initialState :: st
    , handleEvent :: ClSF m EventClock st st
    , simulate :: ClSF m simCl st st
    , renderFrame :: ClSF (RenderT m) renderCl st ()
    }

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
    => InitConfig m st simCl renderCl
    -> m ()
flowSDL InitConfig{..} = do
    SDL.initializeAll
    window <- SDL.createWindow windowTitle windowConfig
    renderer <- SDL.createRenderer window (-1) rendererConfig
    let renderRh :: Rhine m renderCl st ()
        renderRh = hoistClSF ((`runReaderT` RenderEnv{..}) . (.unRenderT)) render @@ waitClock
    flow $
        handleEventOrSim
            >-- keepLast initialState
            --> renderRh
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
    preRender, postRender :: SDL.Renderer -> RenderT m ()
    preRender renderer = do
        SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
        SDL.clear renderer
    postRender renderer = do
        SDL.present renderer
    render :: ClSF (RenderT m) renderCl st ()
    render = proc st -> do
        renderer <- rendererS -< ()
        arrMCl preRender -< renderer
        renderFrame -< st
        arrMCl postRender -< renderer
