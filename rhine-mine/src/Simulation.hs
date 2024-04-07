module Simulation where

import App
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Prelude

simulateS :: (Monad m) => ClSF m cl AppState AppState
simulateS = arrMCl $ execAppT simulate

simulate :: (MutApp m) => m ()
simulate =
    uses #tileOpenQueue Seq.viewl >>= \case
        Seq.EmptyL -> pure ()
        pos :< remainingQueue -> do
            assign #tileOpenQueue remainingQueue
            openTile pos

openTile :: (MutApp m) => Pos -> m ()
openTile pos = unlessM (isTouched pos) do
    modifying #opened $ Set.insert pos
    whenM ((Normal 0 ==) <$> tile pos) do
        neighbours <- filterM (fmap not . isTouched) $ adjacentTiles pos
        modifying #tileOpenQueue (<> Seq.fromList neighbours)
