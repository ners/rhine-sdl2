module Example where

import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import FRP.Rhine hiding (EventClock)
import FRP.Rhine.SDL (flowSDL)
import SDL (($=!))
import SDL qualified
import SDL.Primitive qualified as SDL
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Random (randomIO)
import Prelude

data State = State
    { sides :: Int
    , radius :: Double
    , angle :: Double
    , velocity :: Double
    , colour :: SDL.Color
    , window :: SDL.Window
    , renderer :: SDL.Renderer
    }

handleEvent
    :: (MonadIO m, Tag cl ~ SDL.Event) => ClSFExcept m cl State State ExitCode
handleEvent =
    try $
        tagS &&& returnA >>> arrMCl \(ev, st) -> do
            case ev.eventPayload of
                SDL.QuitEvent -> throwE ExitSuccess
                SDL.MouseButtonEvent (SDL.MouseButtonEventData{..}) | mouseButtonEventMotion == SDL.Pressed -> do
                    (r, b, g) <- randomIO
                    pure st{colour = SDL.V4 r g b 255}
                SDL.MouseWheelEvent
                    (SDL.MouseWheelEventData{mouseWheelEventPos = SDL.V2 _ direction}) ->
                        pure st{radius = max 1 $ st.radius * (1 + fromIntegral direction * 0.1)}
                SDL.KeyboardEvent
                    ( SDL.KeyboardEventData
                            { keyboardEventKeyMotion = SDL.Pressed
                            , keyboardEventRepeat
                            , keyboardEventKeysym =
                                SDL.Keysym{keysymScancode = SDL.Scancode{unwrapScancode = code}}
                            }
                        )
                        | code == 79 -> pure st{velocity = st.velocity + 0.5}
                        | code == 80 -> pure st{velocity = st.velocity - 0.5}
                        | code == 81 && not keyboardEventRepeat ->
                            pure st{sides = max 3 $ st.sides - 1}
                        | code == 82 && not keyboardEventRepeat -> pure st{sides = st.sides + 1}
                _ -> pure st

simulate :: (MonadIO m, Time cl ~ UTCTime) => ClSF m cl State State
simulate =
    sinceLastS &&& returnA >>^ \(δt, st) -> st{angle = st.angle + st.velocity * δt}

renderFrame :: (MonadIO m) => ClSF m cl State ()
renderFrame = arrMCl \State{..} -> do
    SDL.rendererDrawColor renderer $=! SDL.V4 0 0 0 255
    SDL.clear renderer
    (w, h) <-
        (\(SDL.V2 w h) -> (fromIntegral w, fromIntegral h))
            <$> SDL.get (SDL.windowSize window)
    let tau :: (Floating a) => a
        tau = 2 * pi
        pointAngle :: Int -> Double
        pointAngle i = angle + fromIntegral i * tau / fromIntegral sides
        x, y :: Int -> Double
        x i = radius * cos (pointAngle i) + w / 2
        y i = radius * sin (pointAngle i) + h / 2
        xs, ys :: (Integral a, Vector.Storable a) => Vector a
        xs = Vector.generate sides $ round . x
        ys = Vector.generate sides $ round . y
    SDL.fillPolygon renderer xs ys colour
    SDL.present renderer

type SimClock = Millisecond 10

-- 60 FPS => 1000/60 ms/frame
type RenderClock = Millisecond 16

main :: IO ()
main = do
    SDL.initializeAll
    window <-
        SDL.createWindow
            "rhine-sdl2 example"
            SDL.defaultWindow{SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    flowSDL @IO @SimClock @RenderClock
        State
            { sides = 3
            , radius = 200
            , angle = 0
            , velocity = 1
            , colour = SDL.V4 255 255 255 255
            , window
            , renderer
            }
        handleEvent
        simulate
        renderFrame
        >>= exitWith
