{-# LANGUAGE DuplicateRecordFields #-}

module ModelSynthesis where

import Prelude
import App
import System.Random
import System.Random.Stateful
import Data.Hashable (Hashable(hash))
import Data.Sequence qualified as Seq


-- | Generate Floor | Gap randomly, based on previous Floor & Gap
-- | Constraints: 
-- | - Floor can be of width [1, 2, 3, 5, 7-20] where 7+ is the most likely but it's unlikely to be very long
-- | -> y can only increase by one but exactly 3 in a row unless it's at the start of a new floor
-- | -> y can not drop by more than 5
-- | -> y can not increase by more than 2
-- | - Gap can be of width [2-5] with decreasing probability


data GenParams = GenParams
    { seed :: Int
    , maxSpeed :: Float
    , gravity :: Float
    , maxJumpSpeed :: Float
    , minGapWidth :: Int
    , maxGapWidth :: Int
    , maximalDrop :: Int
    }

-- | This function tells us the maximum relative y reached by the player jumping forward by dx
bestJumpHeight :: GenParams -> Float -> Float
bestJumpHeight GenParams{..} dx =
    let t = dx / maxSpeed
     in t * (maxJumpSpeed + t * gravity)

generateRightFloor :: GenParams -> Maybe Floor -> Floor
generateRightFloor _ Nothing = Floor{gapBefore = 0, x = -3, width = 15, y = 0}
generateRightFloor params@GenParams{..} (Just leftFloor) =
    let gen = mkStdGen $ hash (seed, leftFloor.x)
    in runStateGen_ gen \g -> do
        -- TODO: make non-uniform
        gapBefore :: Int <- randomRM (minGapWidth, maxGapWidth) g
        let maxdy = floor . bestJumpHeight params $ fromIntegral gapBefore
        dy :: Int <- randomRM (min (-maximalDrop) (maxdy - maximalDrop), maxdy) g
        let y = leftFloor.y + dy
        let x = leftFloor.x + leftFloor.width + gapBefore
        width :: Int <- randomRM (1, 20) g -- TODO definitely weighted differently ...
        pure Floor{..}

generate :: GenParams -> Int -> Level
generate gen numFloors = Level { floors }
    where
        floors = foldr (\_ acc ->
            generateRightFloor gen (acc Seq.!? 0) <| acc
         ) mempty [0..numFloors]

--generateNext :: TilePattern -> Tile
---- TODO Implement => First item of the first row
--generateNext TilePattern{..}
--    | top == Nothing || left == Nothing = Wall
--
--data Level = Level
--    { width :: Int
--    , height :: Int
--    , rooms :: Map Pos Room
--    }
--
--data Room = Room
--    { width :: Int
--    , height :: Int
--    , tiles :: Map Pos Tile
--    }
--
--emptyRoom :: Int -> Int -> Room
--emptyRoom width height = Room{tiles = mempty, ..}
--
--generate :: (Int, Int) -> Room
--generate (width, height) = foldr genSub (emptyRoom width height) [ Pos{x,y} | x <- [0..width-1], y <- [0..height-1]]
--  where
--    genSub :: Pos -> Room -> Room
--    genSub pos room = do
--        let left = Map.lookup (offsetPos pred id pos) room.tiles
--        let top = Map.lookup (offsetPos id pred pos) room.tiles
--        let topLeft = Map.lookup (offsetPos pred pred pos) room.tiles
--
--        let tile = generateNext TilePattern{..}
--        let tiles = Map.insert pos tile room.tiles
--        room {tiles}
