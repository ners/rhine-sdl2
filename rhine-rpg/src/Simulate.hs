module Simulate where

import App
import Prelude

simulateS :: (Monad m) => ClSF m cl AppState AppState
simulateS = arr simulate

simulate :: AppState -> AppState
simulate state = state
