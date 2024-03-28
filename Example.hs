module Example where

import FRP.Rhine hiding (EventClock)
import FRP.Rhine.SDL qualified as SDL
import SDL qualified
import System.Exit (exitSuccess)
import Prelude

type State = ()

handleEvent :: forall m. (MonadIO m) => ClSF m SDL.EventClock State State
handleEvent = tagS &&& returnA >>> arrMCl (uncurry h)
  where
    h :: SDL.Event -> State -> m State
    h (SDL.eventPayload -> SDL.QuitEvent) = const $ liftIO exitSuccess
    h _ = pure

simulate :: forall m. (MonadIO m) => ClSF m SDL.SimulationClock State State
simulate = returnA

renderFrame :: forall m. (MonadIO m) => ClSF m SDL.RenderClock State ()
renderFrame = returnA

main :: IO ()
main = SDL.flowSDL () handleEvent simulate renderFrame
