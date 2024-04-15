module ModelSynthesis (generate) where

import Prelude

generateNext :: (Maybe Tile, Maybe Tile) -> Tile
-- TODO Implement => First item of the first row
generateNext (Nothing, Nothing) = Wall
-- TODO Implement => Not-first item of the first row
generateNext (Just tileLeft, Nothing) = Wall
-- TODO Implement => First item of a not-first row
generateNext (Nothing, Just tileBottom) = Wall
-- TODO Implement => General case, somewhere in the middle
generateNext (Just leftTile, Just tileBottom) = Air


generate :: (Int, Int) -> [[Tile]]
generate (maxX, maxY) = genSub (0, 0) [[]]
  where
    maybeLeft :: [[Tile]] -> Maybe Tile
    maybeLeft ((h : _) : _) = Just h
    maybeLeft _ = Nothing

    maybeBottom :: [[Tile]] -> Maybe Tile
    maybeBottom [] = Nothing
    maybeBottom [_] = Nothing
    maybeBottom (current : l : _) =
        let i = length current - 1
         in Just $ l !! i

    genSub :: (Int, Int) -> [[Tile]] -> [[Tile]]
    genSub (curX, curY) acc
        | curX < maxX = do
            let newX = generateNext (maybeLeft acc, maybeBottom acc)
            let newAcc = case acc of
                    [] -> [[newX]]
                    h : rest ->
                        let newH = newX : h
                         in newH : rest
            genSub (curX + 1, curY) newAcc
        | curY < maxY =
            -- Current row is full, next one
            -- Add new Y value
            let newAcc = [] : acc
             in genSub (0, curY + 1) newAcc
        | otherwise = acc
