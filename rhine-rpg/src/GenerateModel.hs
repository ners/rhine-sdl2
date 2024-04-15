module GenerateModel where

import Prelude

repl :: (Int -> a) -> Int -> [a]
repl f r
    | r > 0 = f r : repl f (r - 1)
    | otherwise = []

randomTile :: Int -> Int -> Tile
randomTile i j
    | i `mod` 2 /= j `mod` 2 = Air
    | otherwise = Wall

initialMapSize :: Int
initialMapSize = 100

generate :: [[Tile]]
generate = repl (\i -> repl (randomTile i) initialMapSize) initialMapSize
