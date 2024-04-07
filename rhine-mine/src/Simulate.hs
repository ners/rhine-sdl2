module Simulate where

import App
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Prelude

simulateS :: (Monad m) => ClSF m cl AppState AppState
simulateS = arr simulate

simulate :: AppState -> AppState
simulate state =
    case Seq.viewl state.tileOpenQueue of
        Seq.EmptyL -> state
        pos :< remainingQueue
            | Set.member pos state.opened || Set.member pos state.flags ->
                state{tileOpenQueue = remainingQueue}
        pos :< remainingQueue ->
            let tileToOpen = tile state pos
                neighboursToOpen =
                    Seq.fromList
                        [ npos | tileToOpen == Normal 0, npos <- adjacentTiles pos, not (Set.member npos state.opened)
                        ]
             in state
                    { opened = Set.insert pos state.opened
                    , tileOpenQueue = remainingQueue <> neighboursToOpen
                    }
